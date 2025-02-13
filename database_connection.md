Beginning with the 2024 Annual Report, the team adopted a shorthand method to connect to our databases. This method allows AKRO and AFSC members to call their own specific database connections using `ROracle` or `odbc`.

Create your .Renviron file by running: 

**`usethis::edit_r_environ()`**

This creates a `.Renviron` file in your documents folder, but you will be able to edit it via Rstudio. This is a local file that is OUTSIDE of your gitHub repositories so you shouldn't be able to accidentally add your information to a repo. 

AFSC members will add the following to the `.Renviron` file (note that it doesn't use R syntax strictly):
```
AFSCid = <your AFSC database ID, no quotes needed>
AFSCpw = <your AFSC database password>
channel_afsc = "library(odbc); dbConnect(drv = odbc::odbc(), dsn = 'AFSC', UID = Sys.getenv('AFSCid'), PWD = Sys.getenv('AFSCpw'))"
```

Save the file, close it, and then restart R, as `.Renviron` is loaded at the start of your R sessions.

Now, anytime you run:

**`channel_afsc <- eval(parse(text = Sys.getenv('channel_afsc')))`**

you should automatically load the `odbc` library, connect to the AFSC database, and assign your connection all in one.
