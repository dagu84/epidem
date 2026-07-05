import pandas as pd
import pytest
from package.data import process_to_triangle, to_mem_format, mem_and_phase_labels


def make_linelist():
    return pd.DataFrame({
        'date_onset':  ['2020-01-06', '2020-01-06', '2020-01-13'],
        'date_report': ['2020-01-13', '2020-01-20', '2020-01-20'],
        'Delay':       [1, 2, 1],
        'CLASSI_FIN':  [4.0, 4.0, 4.0],
        'EVOLUCAO':    [1.0, 1.0, 1.0],
        'CO_MUN_RES':  [310620, 310620, 310620],
    })


def make_triangle():
    dates = pd.date_range('2021-01-04', periods=104, freq='W-MON')
    return pd.DataFrame({'date': dates, 'n_td': range(104)})


def make_weekly(n=10):
    return pd.DataFrame({
        'date': pd.date_range('2020-01-06', periods=n, freq='W-MON'),
        'n_td': [10, 20, 500, 800, 1500, 1700, 900, 400, 100, 50][:n],
    })


class TestProcessToTriangle:

    def test_output_columns(self):
        result = process_to_triangle(make_linelist())
        assert list(result.columns) == ['date', 't', 'd', 'week_of_year', 'n_td']

    def test_delay_filter(self):
        result = process_to_triangle(make_linelist(), d=1)
        assert result['d'].max() <= 1

    def test_complete_grid(self):
        result = process_to_triangle(make_linelist(), d=2)
        counts = result.groupby('date')['d'].count()
        assert (counts == 3).all()

    def test_counts_correct(self):
        result = process_to_triangle(make_linelist(), d=2)
        val = result.loc[(result['date'] == '2020-01-06') & (result['d'] == 1), 'n_td'].iloc[0]
        assert val == 1

    def test_n_td_dtype_is_int(self):
        result = process_to_triangle(make_linelist())
        assert result['n_td'].dtype == int

    def test_week_of_year_no_53(self):
        result = process_to_triangle(make_linelist())
        assert (result['week_of_year'] != 53).all()

    def test_cases_exceeding_d_are_excluded(self):
        df = make_linelist()
        df.loc[0, 'Delay'] = 99
        result = process_to_triangle(df, d=2)
        assert result['n_td'].sum() < make_linelist().shape[0]


class TestToMemFormat:

    def test_output_has_52_rows(self):
        result = to_mem_format(make_triangle())
        assert result.shape[0] == 52

    def test_column_names_prefixed(self):
        result = to_mem_format(make_triangle())
        assert all(c.startswith('Season_') for c in result.columns)

    def test_incomplete_years_dropped(self):
        dates = pd.date_range('2021-01-04', periods=53, freq='W-MON')
        df = pd.DataFrame({'date': dates, 'n_td': 1})
        result = to_mem_format(df)
        assert result.shape[1] == 1

    def test_epiweek_index_range(self):
        result = to_mem_format(make_triangle())
        assert result.index.name == 'epiweek'
        assert result.index.min() >= 1
        assert result.index.max() <= 52

    def test_aggregates_triangle_input(self):
        df = pd.DataFrame({
            'date': ['2020-01-06', '2020-01-06'],
            'n_td': [3, 7],
        })
        # Single week → dropped as incomplete year; check no error raised
        to_mem_format(df)

    def test_two_seasons_produce_two_columns(self):
        result = to_mem_format(make_triangle())
        assert result.shape[1] == 2


class TestMemAndPhaseLabels:

    def test_output_columns_present(self):
        result = mem_and_phase_labels(make_weekly(), moderate=100, high=1000, very_high=1500)
        assert {'regime', 'trend', 'phase'}.issubset(result.columns)

    def test_regime_values_are_valid(self):
        result = mem_and_phase_labels(make_weekly(), moderate=100, high=1000, very_high=1500)
        assert set(result['regime'].unique()).issubset({'baseline', 'moderate', 'high', 'very high'})

    def test_phase_values_are_valid(self):
        result = mem_and_phase_labels(make_weekly(), moderate=100, high=1000, very_high=1500)
        assert set(result['phase'].unique()).issubset({'rising', 'declining', 'stable'})

    def test_baseline_regime(self):
        df = pd.DataFrame({'date': pd.date_range('2020-01-06', periods=4, freq='W-MON'), 'n_td': [10, 10, 10, 10]})
        result = mem_and_phase_labels(df, moderate=50, high=100, very_high=200)
        assert (result['regime'] == 'baseline').all()

    def test_very_high_regime(self):
        df = pd.DataFrame({'date': pd.date_range('2020-01-06', periods=4, freq='W-MON'), 'n_td': [300, 300, 300, 300]})
        result = mem_and_phase_labels(df, moderate=50, high=100, very_high=200)
        assert (result['regime'] == 'very high').all()

    def test_rising_phase(self):
        df = pd.DataFrame({'date': pd.date_range('2020-01-06', periods=6, freq='W-MON'), 'n_td': [10, 20, 30, 40, 50, 60]})
        result = mem_and_phase_labels(df, moderate=100, high=200, very_high=300)
        assert (result['phase'].iloc[3:] == 'rising').all()

    def test_declining_phase(self):
        df = pd.DataFrame({'date': pd.date_range('2020-01-06', periods=6, freq='W-MON'), 'n_td': [60, 50, 40, 30, 20, 10]})
        result = mem_and_phase_labels(df, moderate=100, high=200, very_high=300)
        assert (result['phase'].iloc[3:] == 'declining').all()
