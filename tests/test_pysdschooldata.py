"""
Tests for pysdschooldata Python wrapper.

Minimal smoke tests - the actual data logic is tested by R testthat.
These just verify the Python wrapper imports and exposes expected functions.
"""

import pytest


def test_import_package():
    """Package imports successfully."""
    import pysdschooldata
    assert pysdschooldata is not None


def test_has_fetch_enr():
    """fetch_enr function is available."""
    import pysdschooldata
    assert hasattr(pysdschooldata, 'fetch_enr')
    assert callable(pysdschooldata.fetch_enr)


def test_has_get_available_years():
    """get_available_years function is available."""
    import pysdschooldata
    assert hasattr(pysdschooldata, 'get_available_years')
    assert callable(pysdschooldata.get_available_years)


def test_has_version():
    """Package has a version string."""
    import pysdschooldata
    assert hasattr(pysdschooldata, '__version__')
    assert isinstance(pysdschooldata.__version__, str)
