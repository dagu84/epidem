import pandas as pd
import pytest
from unittest.mock import patch, MagicMock
from package.model import run_mem, create_and_train_gam, create_and_train_inla, CODING_DIR


def mock_success():
    m = MagicMock()
    m.returncode = 0
    return m


def mock_failure():
    m = MagicMock()
    m.returncode = 1
    m.stderr = "R error"
    return m


class TestRunMem:

    @patch('package.model.pd.read_csv')
    @patch('package.model.subprocess.run')
    def test_returns_dataframe_on_success(self, mock_run, mock_csv):
        mock_run.return_value = mock_success()
        mock_csv.return_value = pd.DataFrame({'threshold': [100, 200]})
        result = run_mem('mem.r', '/data/input.csv', 'sari')
        assert isinstance(result, pd.DataFrame)

    @patch('package.model.subprocess.run')
    def test_returns_none_on_failure(self, mock_run):
        mock_run.return_value = mock_failure()
        result = run_mem('mem.r', '/data/input.csv', 'sari')
        assert result is None

    @patch('package.model.pd.read_csv')
    @patch('package.model.subprocess.run')
    def test_passes_correct_args_to_rscript(self, mock_run, mock_csv):
        mock_run.return_value = mock_success()
        mock_csv.return_value = pd.DataFrame()
        run_mem('mem.r', '/data/input.csv', 'sari')
        mock_run.assert_called_once_with(
            ['Rscript', str(CODING_DIR / 'mem.r'), '/data/input.csv', 'sari'],
            capture_output=True, text=True)


class TestCreateAndTrainGam:

    @patch('package.model.pd.read_csv')
    @patch('package.model.subprocess.run')
    def test_returns_three_dataframes_on_success(self, mock_run, mock_csv):
        mock_run.return_value = mock_success()
        mock_csv.return_value = pd.DataFrame({'date': [], 'pred': []})
        pred, ci, posterior = create_and_train_gam('gam.r', '/data/input.csv', 40)
        assert all(isinstance(df, pd.DataFrame) for df in [pred, ci, posterior])

    @patch('package.model.subprocess.run')
    def test_returns_none_on_failure(self, mock_run):
        mock_run.return_value = mock_failure()
        result = create_and_train_gam('gam.r', '/data/input.csv', 40)
        assert result is None

    @patch('package.model.pd.read_csv')
    @patch('package.model.subprocess.run')
    def test_passes_correct_args_to_rscript(self, mock_run, mock_csv):
        mock_run.return_value = mock_success()
        mock_csv.return_value = pd.DataFrame()
        create_and_train_gam('gam.r', '/data/input.csv', 40)
        mock_run.assert_called_once_with(
            ['Rscript', str(CODING_DIR / 'gam.r'), '/data/input.csv', '40'],
            capture_output=True, text=True)

    @patch('package.model.pd.read_csv')
    @patch('package.model.subprocess.run')
    def test_cutoff_passed_as_string(self, mock_run, mock_csv):
        mock_run.return_value = mock_success()
        mock_csv.return_value = pd.DataFrame()
        create_and_train_gam('gam.r', '/data/input.csv', 40)
        call_args = mock_run.call_args[0][0]
        assert call_args[3] == '40'


class TestCreateAndTrainInla:

    @patch('package.model.pd.read_csv')
    @patch('package.model.subprocess.run')
    def test_returns_two_dataframes_on_success(self, mock_run, mock_csv):
        mock_run.return_value = mock_success()
        mock_csv.return_value = pd.DataFrame({'date': [], 'pred': []})
        pred, posterior = create_and_train_inla('inla.r', '/data/input.csv', 40)
        assert all(isinstance(df, pd.DataFrame) for df in [pred, posterior])

    @patch('package.model.subprocess.run')
    def test_returns_none_on_failure(self, mock_run):
        mock_run.return_value = mock_failure()
        result = create_and_train_inla('inla.r', '/data/input.csv', 40)
        assert result is None

    @patch('package.model.pd.read_csv')
    @patch('package.model.subprocess.run')
    def test_passes_correct_args_to_rscript(self, mock_run, mock_csv):
        mock_run.return_value = mock_success()
        mock_csv.return_value = pd.DataFrame()
        create_and_train_inla('inla.r', '/data/input.csv', 40)
        mock_run.assert_called_once_with(
            ['Rscript', str(CODING_DIR / 'inla.r'), '/data/input.csv', '40'],
            capture_output=True, text=True)
