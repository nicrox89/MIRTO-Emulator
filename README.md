# MIRTO-Emulator


 A racket language base project to build an emulator for the Middlesex RoboTic platfOrm (MIRTO).
 The purpose, especially during the COVID-19 lockdown where the students were forced to stay at home,
 is to allow students the testing of their racket code without having a physical MIRTO robot.
 To simplify the usage of this emulator, there were used only the basic library included with the
 standard installation of Drracket.

![MIRTO mapping](mapping.png)


The MirtoEmulator library use two additional files:
- beep.wav
- bg.png

It is possible to replace the bg file drawing a black line on a white background.
Background image information:

- WIDTH 500px
- HEIGHT 500px
- settings bitmap 1bit
- brush 6 black
- background white
- format png

# GUI sections

The GUI is composed by two main areas, the bot area on the left and the control panel area on the right.
On the botton there is a status bar that indicate the sensor's state.

## Control Panel

The control panel is composed by the display, the button panels (potentiometer and push-button), the graphical wheels status, the bumpers control and the infrared gui status. 
It is possible to interact using the mouse with:
- potentiometer: dragging up or down with the mouse (value range 0-1023)
- push-button: left click on it to press the button, when released the button is resetted
- bumpers: clicking on left, center, right it is possible to press one or both the bump sensors. If it is necessary to press for long time the dragging option is enabled

## Bot area

The bot area represents the bot movements. The start position is set at x:80 y:300 z:0 close the black line but it is possible to interact to change the coordinates and the direction.
Clicking on the bot with the left mouse button the positioning mode is enabled, to place the bot simply move it in the positioning mode and click again the mouse left button to replace it. It is possible to rotate the bot in the positioning mode clicking the mouse right button. It is possible to choose between height positions (0° 45° 90° 135° 180° 225° 270° 315°).
