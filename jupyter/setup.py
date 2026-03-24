from setuptools import setup

setup(
    name='zoa_kernel',
    version='0.1.0',
    description='Jupyter kernel for Zoa optical design engine',
    packages=['zoa_kernel'],
    install_requires=[
        'jupyter_client',
        'ipykernel',
        'pyzmq',
    ],
    python_requires='>=3.8',
)
