"""Entry point for ``python -m zoa_kernel``."""
from ipykernel.kernelapp import IPKernelApp
from .kernel import ZoaKernel

IPKernelApp.launch_instance(kernel_class=ZoaKernel)
