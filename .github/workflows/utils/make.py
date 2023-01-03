import fire
import subprocess
from glob import glob 
import natsort  
from pathlib import Path

def get_all_targets():
    """Get all makefiles. Assumes that it is run from the root of the repository."""
    return natsort.natsorted(list(glob("*/**/Makefile")))

def clean_all():
    """Clean all makefiles.
    
    Example:
        >>  python .github/workflows/utils/make.py clean_all
    """
    for makefile in get_all_targets():
        print(f"Cleaning {makefile}...")
        subprocess.run(["make", makefile, "clean"], cwd=Path(makefile).parent)

def build_all():
    """Build all makefiles.
    
    Example:
        >>  python .github/workflows/utils/make.py build_all
    """
    for makefile in get_all_targets():
        print(f"Building {makefile}...")
        subprocess.run(["make",  makefile], cwd=Path(makefile).parent)

if __name__ == "__main__":
    fire.Fire()