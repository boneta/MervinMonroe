### [DEPRECATED] :no_entry::warning::skull:

This project, intended as an initial attempt to simplify QM/MM calculations, has reached the end and will no longer be updated or maintained.

Its evolution can be found in [DYNAMO<sup>N</sup>](http://github.com/boneta/dynamon) and [JACQUES](http://github.com/boneta/jacques)



# Mervin Monroe

*A lazy interface for fDynamo software*

### Usage
```
  mervinmonroe <subprogram> [options]
```

### Subprograms

  - info
  - update
  - import
  - compile
  - minimization
  - pel / scan
  - pes
  - locate
  - irc
  - pmf
  - correction
  - dynamics
  - tools


### Installation / Configuration

  1. Download and place the software in your preferred destination folder.
  2. Source the file `.mervinconfig` (This will compile fDynamo, so it may take a while)

        example:  `source /home/user/bin/mervinmonroe/.mervinconfig`

  3. Not compulsory but strongly suggested:  
  Include the *source* statement in your `.bashrc`


### Update

To automatically download and install a newer version of *MervinMonroe*, just type: `mervinmonroe update`
Nothing more! (fDynamo will be compiled again, so it may take a while)


### Download

At destination folder:  
`git clone https://github.com/boneta/mervinmonroe.git`
