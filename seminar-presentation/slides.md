% GPGPU
% Philip Dexter
% \today

# GPGPU
* GPGPU based on Intel's Larrabee
* Schedule tasks via software instead of hardware
* "The aim of this project is to explore the performance of this type of architecture using a cycle accurate hardware model"

# Where I come in

* Some code changes
	* Performance counters
	* Simple stuff
* Run experiments
* Summarize related work

# Thread switching

* Two thread switching strategies
	* Barrel switching
	* Switch on stall

# Rollbacks

* Six causes of rollbacks
* Right now focusing on explaining cache miss rollback in context of barrel vs SoS switching

# Near future

* Sensitivity test on cache associativity

# Improving the Load/Store Queue

![](barrel-vs-sos.pdf)

# Improving the Load/Store Queue

![](rollback-reduce.pdf)

# Varying Strands Per Core

![](strands-per-core.pdf)

# Rasterization: Hardware vs. Software

![](hardware-vs-software.pdf)

# Varying Cache Size and Associativity

![](cache-associativity-speedup.pdf)

# --

- -
