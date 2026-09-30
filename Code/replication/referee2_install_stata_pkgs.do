* referee2_install_stata_pkgs.do
* Referee's own setup script (does not touch author code).
* Installs reghdfe + ftools for high-dimensional FE replication.
capture noisily ssc install ftools, replace
capture noisily ssc install reghdfe, replace
which reghdfe
which ftools
display "INSTALL_DONE"
