import os
from shutil import copy
import pathlib
import glob

from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext as build_ext_orig

class CMakeExtension(Extension):

    def __init__(self, name):
        # don't invoke the original build_ext for this special extension
        super().__init__(name, sources=[])


class build_ext(build_ext_orig):

    def run(self):
        for ext in self.extensions:
            self.build_cmake(ext)

    def build_cmake(self, ext):
        cwd = pathlib.Path().absolute()

        build_temp = pathlib.Path(self.build_temp)
        build_temp.mkdir(parents=True, exist_ok=True)
        extdir = pathlib.Path(self.get_ext_fullpath(ext.name)).parent
        extdir.mkdir(parents=True, exist_ok=True)

        cmake_args = [
            '-DCMAKE_BUILD_TYPE=DEBUG',
            '-DFortran_COMPILER=mpifort',
            '-DPYTHON_INTERFACE=on',
        ]

        os.chdir(str(build_temp))
        self.spawn(['cmake', str(cwd)] + cmake_args)
        if not self.dry_run:
            self.spawn(['make'])

        for file in glob.glob('quicklb_lib.*.so'):
          print("Copying: ", file)
          copy(file, str(cwd)+"/"+str(extdir))
        os.chdir(str(cwd))

setup(
    name='quicklb',
    version='0.1',
    packages=['quicklb'],
    ext_modules=[CMakeExtension('quicklb_lib')],
    cmdclass={
        'build_ext': build_ext,
    },
)

