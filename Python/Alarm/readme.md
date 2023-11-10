# ALARM PROGRAMMING IN PYTHON
### Functioning:
1 - We define the date and time of the alarm in the `target_date` variable (the `datetime` library must be imported)

2 - We start an infinite loop and in this loop,
> 2.1 - We constantly retrieve the current date and time in the `current_date` variable
>
> 2.2 - If we are on the defined date, we start the ringtone with the Media Playback program (in this case `parole` for the LINUX OS - in our case, or `MusicUI.exe` for Windows)
>
> 2.3 - We break the loop and exit the program after starting the ringtone
