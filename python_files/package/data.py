import pandas as pd
from pathlib import Path


def export(df:pd.DataFrame, file_name:str):
    """Function to save dataframe into the correct data folder within the repository"""
    base_dir = Path(__file__).resolve().parent
    data_dir = base_dir.parents[1] / 'data'

    data_dir.mkdir(parents=True, exist_ok=True)

    file_path = data_dir / f'{file_name}.csv'
    df.to_csv(file_path, index=False)
    return print(f'executed, file {file_name} saved to {file_path}.csv')


def process_to_triangle(df:pd.DataFrame, d:int=10):
    """
    Convert a line-list of case data into a reporting-delay triangle.

    Cleans the input by dropping unused columns, parses the onset and report
    dates, and filters to cases with a reporting delay of at most `d` weeks.
    Cases are then aggregated by onset date and delay into counts, and the
    result is expanded onto a complete weekly date x delay grid (filling
    missing combinations with zero counts) so that every date has an entry
    for every delay from 0 to d. Adds a sequential time index (`t`) and
    ISO week-of-year feature for downstream modelling.

    Parameters
    ----------
    df : pd.DataFrame
        Line-list data containing at least 'Delay', 'date_onset', and
        'date_report' columns, along with 'CLASSI_FIN', 'EVOLUCAO', and
        'CO_MUN_RES' (which are dropped).
    d : int
        Maximum reporting delay (in weeks) to retain. Cases with a longer
        delay are excluded.

    Returns
    -------
    pd.DataFrame
        A complete weekly date x delay grid with columns:
        'date', 't' (sequential time index), 'd' (delay in weeks),
        'week_of_year', and 'n_td' (case count for that date/delay pair,
        zero-filled where no cases were observed).
    """
    data = df.copy()
    data = data.rename(columns={'Delay': 'delay'}).drop(columns=['CLASSI_FIN', 'EVOLUCAO', 'CO_MUN_RES'])

    # changing data format
    data['date_onset']  = pd.to_datetime(data['date_onset'])
    data['date_report'] = pd.to_datetime(data['date_report'])

    # keeping the number of delays (weeks) specified
    n_before = len(data)
    data = data[data['delay'] <= d].copy()
    print(f"\nKept {len(data)} / {n_before} cases with Delay ≤ {d}, {100*len(data)/n_before:.1f}%")
    data = data.rename(columns={'delay':'d', 'date_onset':'date'})

    #
    counts = (data.groupby(['date', 'd']).size().reset_index(name='n_td'))
    all_dates = pd.date_range(counts['date'].min(), counts['date'].max(), freq='W-MON')
    grid = pd.MultiIndex.from_product([all_dates, range(d + 1)],names=['date', 'd']).to_frame(index=False)
    data = grid.merge(counts, on=['date', 'd'], how='left').fillna({'n_td': 0})

    # creating time feature
    data['t'] = data['date'].rank(method='dense').astype(int)
    data['week_of_year'] = data['date'].dt.isocalendar().week.astype(int)
    data.loc[data['week_of_year'] == 53, 'week_of_year'] = 52

    # filtering to the selected columns
    data = data[['date', 't', 'd', 'week_of_year', 'n_td']]
    data['n_td'] = data['n_td'].astype(int)

    return data


def to_mem_format(df:pd.DataFrame, date_col:str="date", value_col:bool="n_td"):
    """
    Transform a long-format case-count dataframe into the wide format
    required by R's `mem` package: one row per ISO epidemiological week
    (1-52), one column per season (calendar year). Removes any incomplete years (weeks missing)

    Parameters
    ----------
    df : pd.DataFrame
        Long-format dataframe. Can be cell-level (one row per date+delay)
        or already aggregated to one row per date - both are handled.
    date_col : str
        Column containing the date for each row.
    value_col : str
        Column to sum and pivot (e.g. 'n_td' for the eventual truth).

    Returns
    -------
    pd.DataFrame
        Wide dataframe: index = epiweek (1-52), columns = 'Season_<year>'.
    """
    df = df.copy()
    df[date_col] = pd.to_datetime(df[date_col])

    # Aggregate to one row per date (safe no-op if already aggregated)
    weekly = df.groupby(date_col, as_index=False)[value_col].sum()

    # ISO year/week - correctly handles year-boundary weeks
    # (e.g. late Dec dates that belong to ISO week 1 of the following year)
    iso = weekly[date_col].dt.isocalendar()
    weekly["epiyear"] = iso["year"]
    weekly["epiweek"] = iso["week"]

    # Fold rare ISO week 53 into week 52 for a consistent 1-52 grid
    weekly.loc[weekly["epiweek"] == 53, "epiweek"] = 52

    # Pivot: rows = epiweek, columns = epiyear
    wide = weekly.pivot_table(
        index="epiweek", columns="epiyear", values=value_col, aggfunc="sum")

    # Ensure every week 1-52 is present even if some are missing data
    wide = wide.reindex(range(1, 53)).sort_index()
    wide.columns = [f"Season_{int(c)}" for c in wide.columns]

    # Removing any incomplete years
    wide = wide.dropna(axis=1)

    return wide


def mem_and_phase_labels(df:pd.DataFrame, moderate:float, high:float, very_high:float):
    """
    Classify weekly case counts into intensity regimes and trend phases.

    Expects a pre-aggregated weekly dataframe (one row per date). Assigns
    each week a `regime` label ('baseline', 'moderate', 'high', 'very high')
    based on fixed thresholds applied to `n_td`. Also computes a 3-week
    trend (`n_td` change over a 3-week lag) and classifies each week's
    `phase` as 'rising', 'declining', or 'stable' based on the sign of
    that trend.

    Parameters
    ----------
    df : pd.DataFrame
        Pre-aggregated weekly data containing 'date' and 'n_td' columns
        (one row per date).
    moderate : float
        Lower threshold for the 'moderate' regime.
    high : float
        Lower threshold for the 'high' regime.
    very_high : float
        Lower threshold for the 'very high' regime.

    Returns
    -------
    pd.DataFrame
        Weekly data with columns: 'date', 'n_td', 'regime', 'trend',
        and 'phase'.
    """

    # create the regime column using statements
    df['regime'] = df['n_td'].apply(lambda x: 'baseline' if x < moderate
        else 'moderate' if x < high
        else 'high' if x < very_high
        else 'very high')

    # creating the 3 phases of cases
    df = df.sort_values('date').copy()
    df['trend'] = df['n_td'].diff(3)  # change over `window` weeks
    df['phase'] = df['trend'].apply(
        lambda x: 'rising' if x > 0 else 'declining' if x < 0 else 'stable')

    return df


def functions():
    """
    """
    return None
