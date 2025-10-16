{
  boot.extraModprobeConfig = "options kvm_amd nested=1";
  hardware.cpu.amd.updateMicrocode = true;
}
