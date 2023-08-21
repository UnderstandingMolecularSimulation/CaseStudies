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
        >>  python make.py clean_all
    """
    for makefile in get_all_targets():
        print(f"Cleaning {makefile}...")
        subprocess.run(["make", "clean"], cwd=Path(makefile).parent)

def build_all():
    """Build all makefiles.
    
    Example:
        >>  python make.py build_all
    """
    for makefile in get_all_targets():
        if any(word in makefile for word in ['CaseStudy_' + index for index in ['19', '20', '21', '22', '23', '24', '25']]):
            continue
        print(f"Building {makefile}...")
        completed_process = subprocess.run(["make"], cwd=Path(makefile).parent)
        if completed_process.returncode != 0:
            raise Exception(f"Failed to build {makefile}")
        

if __name__ == "__main__":
    fire.Fire()