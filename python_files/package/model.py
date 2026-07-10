import os
import subprocess
import numpy as np
import pandas as pd
from pathlib import Path

# coding/ dir (two levels up from this file: package/ -> python_files/ -> coding/)
CODING_DIR = Path(__file__).resolve().parent.parent.parent
DATA_DIR = CODING_DIR / 'data'


def _resolve_r_path(r_path: str) -> str:
    path = Path(r_path)
    return str(path if path.is_absolute() else CODING_DIR / path.name)


def _resolve_data_path(data_path: str) -> str:
    path = Path(data_path)
    return str(path if path.is_absolute() else DATA_DIR / path.name)


def run_mem(r_path:str, data_path:str, disease:str):
    """
    Run the MEM threshold model in R for a given disease.
    Passes the data path and disease name as command-line arguments to the R
    script. R fits the MEM model and writes the threshold outputs to CSV.
    Python then reads those back.

    Parameters
    ----------
    r_path : string file name of the R file to be executed (found in coding/)
    data_path : string file name of the data to be run in the R file (found in coding/data/)
    disease : str, disease name used to label the R script output CSV

    Returns
    -------
    df : pd.DataFrame with MEM intensity thresholds
    """
    r_path = _resolve_r_path(r_path)
    data_path = _resolve_data_path(data_path)

    # running file script
    output = subprocess.run(
        ['Rscript', f'{r_path}', data_path, disease],
        capture_output=True, text=True)

    # testing if the file ran successfully
    if output.returncode == 0:
        print(f'{r_path} finished successfully')
    else:
        print(f'{r_path} ran into an error:\n')
        print(output.stdout)
        print(output.stderr)
        return None

    # save the R output
    df = pd.read_csv(os.path.join(os.path.dirname(data_path), f"mem_threshold_{disease}.csv"))

    return df


def create_and_train_gam(r_path:str, data_path:str, end_cutoff:int):
    """
    Run the GAM nowcasting model in R for a given T_cutoff.
    Passes the cutoff value as a command-line argument to the R script.
    R reads the data, fits the model, and writes the nowcast summary
    and posterior samples to CSV. Python then reads those back.

    Parameters
    ----------
    r_path : string file name of the R file to be executed (found in coding/)
    data_path : string file name of the data to be run in the R file (found in coding/data/)
    cutoff : int, week number to be used to cutoff and predicted after

    Returns
    -------
    pred : pd.DataFrame with model predictions per day per delay
    ci : pd.DataFrame with model credible interval per day
    posterior : pd.DataFrame with posterior distribution (1000 smaples per day)
    """
    r_path = _resolve_r_path(r_path)
    data_path = _resolve_data_path(data_path)
    start_cutoff = int(end_cutoff - 30)

    # running the file script
    output = subprocess.run(
        ['Rscript', f'{r_path}', data_path, str(start_cutoff), str(end_cutoff)],
        capture_output=True, text=True)

    # testing if the file ran successfully
    if output.returncode == 0:
        print(f'{r_path} finished successfully')
    else:
        print(f'{r_path} ran into an error:\n')
        print(output.stdout)
        print(output.stderr)
        return None

    # save the R output
    pred = pd.read_csv(os.path.join(os.path.dirname(data_path), f"gam_pred_{end_cutoff}.csv"),parse_dates=["date"])
    ci = pd.read_csv(os.path.join(os.path.dirname(data_path), f"gam_ci_{end_cutoff}.csv"),parse_dates=["date"])
    posterior = pd.read_csv(os.path.join(os.path.dirname(data_path), f"gam_posterior_{end_cutoff}.csv"),index_col=0)

    return pred, ci, posterior


def create_and_train_inla(r_path:str, data_path:str, end_cutoff:int):
    """
    Run the INLA nowcasting model in R for a given T_cutoff.
    Passes the cutoff value as a command-line argument to the R script.
    R reads the data, fits the model, and writes the nowcast summary
    and posterior samples to CSV. Python then reads those back.

    Parameters
    ----------
    r_path : string file name of the R file to be executed (found in coding/)
    data_path : string file name of the data to be run in the R file (found in coding/data/)
    cutoff : int, week number to be used to cutoff and predicted after

    Returns
    -------
    pred : pd.DataFrame with model predictions per day per delay
    posterior : pd.DataFrame with posterior distribution per day
    """
    r_path = _resolve_r_path(r_path)
    data_path = _resolve_data_path(data_path)
    start_cutoff = int(end_cutoff - 30)

    # running the file script
    output = subprocess.run(
        ['Rscript', f'{r_path}', data_path, str(start_cutoff), str(end_cutoff)],
        capture_output=True, text=True)

    # testing if the file ran successfully
    if output.returncode == 0:
        print(f'{r_path} finished successfully')
    else:
        print(f'{r_path} ran into an error:\n')
        print(output.stdout)
        print(output.stderr)
        return None

    # save the R output
    pred = pd.read_csv(os.path.join(os.path.dirname(data_path), f"inla_pred_{end_cutoff}.csv"),parse_dates=["date"])
    posterior = pd.read_csv(os.path.join(os.path.dirname(data_path), f"inla_posterior_{end_cutoff}.csv"),index_col=0)

    return pred, posterior


def crps_eval(draws, y_obs):
    """
    draws: (n_draws, n_cells) posterior draws
    y_obs: (n_cells,) observed values (the NaN triangle, now realized/held out)
    returns: (n_cells,) CRPS per cell
    """
    draws = np.sort(draws, axis=0)
    n = draws.shape[0]
    i = np.arange(1, n + 1)[:, None]
    term1 = np.mean(np.abs(draws - y_obs[None, :]), axis=0)
    term2 = np.sum((2 * i - n - 1) * draws, axis=0) / (n ** 2)

    return term1 - term2


def rmse_eval(pred, y_obs):
    """
    pred: (n_cells,) predicted values
    y_obs: (n_cells,) observed values (the NaN triangle, now realized/held out)
    returns: (n_cells,) RMSE per cell
    """
    return np.sqrt(np.mean((pred - y_obs) ** 2, axis=0))
