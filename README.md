# parking_lot

**Usage**
- `bin/setup` compiles the code, runs the unit tests and gives code coverage
- `bin/parking_lot` runs the code taking input from shell
- `bin/parking_lot File` runs the code taking input from a file named `File`

*Few things to note:*
- parking lot's state is not persistent across reboots.
- input can either be from shell or from a file followed by shell input if required.
- the nearest vacant spot is reported when the system tries to find a free parking spot.
- all the processing operations other than 'create_parking_lot' parallelised
- parallelisation of user requests stops for the instant when a "create_parking_lot" request is encountered and is being served. 'parallel serving' starts again right after it is served.
- create_parking_lot can be called multiple times during a single session. requested number of parking spots get added to the existing lot.
- tested for a million-big parking lot on an 8GB machine successfully.
