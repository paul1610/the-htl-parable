/* the htl parable, by Leon Leeb, Paul Nell, Niklas Trinkl. */

:- dynamic i_am_at/1, at/2, holding/1, get_name/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(get_name(_)).

/* defines the start location */

i_am_at(entrance).

/* These facts describe how the rooms are connected. */

path(entrance, stay). /* 0 -> 0.1 */
path(entrance, right01). /* 0 -> 0.2 */
path(entrance, stairs). /* 0 -> 1 */

path(stay, right01). /* 0.1 -> 0.2 */
path(stay, stairs). /* 0.1 -> 1 */

path(right01, hallway01). /* 0.2 -> 0.3 */
path(right01, slightlyOpenedDoor). /* 0.2 -> 0.2.0 */
path(right01, stairs). /* 0.2 -> 1 */

path(slightlyOpenedDoor, hallway01). /* 0.2.0 -> 0.3 */
path(slightlyOpenedDoor, stairs). /* 0.2.0 -> 1 */

path(hallway01, door01). /* 0.3 -> 0.4 */
path(hallway01, teacher01). /* 0.3 -> 0.3.0 */

path(door01, panic). /* 0.4 -> 0.3.1 */

path(teacher01, panic). /* 0.3.0 -> 0.3.1 */
path(teacher01, office). /* 0.3.0 -> 1.1 */

path(panic, _). /* 0.3.1 -> TODO: RESET */

path(stairs, teacher02). /* 1 -> 1.0 */
path(stairs, ignore). /* 1 -> 2 */

path(teacher02, office). /* 1.0 -> 1.1 */

path(office, coffee). /* 1.1 -> 1.2 */
path(office, class). /* 1.1 -> 3 */

path(coffee, _). /* 1.2 -> TODO: RESET ;- die. */
path(coffe, exit). /* 1.2 -> 1.3 */

path(exit, exploreOnYourOwn). /* 1.3 -> 1.3.0 */
path(exploreOnYourOwn, hideInToilet). /* 1.3.0 -> 1.3.0.0 */
path(hideInToilet, _). /* 1.3.0.0 -> 1.3.4 RESET */ 
path(exploreOnYourOwn, goToLightSaber). /* 1.3.0 -> 1.3.1 */ 
path(goToLightSaber, _). /* 1.3.1 -> 1.3.4 RESET */
path(exit, trustNarrator). /* 1.3 -> 1.4 */

path(trustNarrator, hideInToilet). /* 1.4 -> 1.3.0.0 */
path(trustNarrator, findButton). /* 1.4 -> 1.5 */

path(findButton, runWithOutNarrator).  /* 1.5 -> 1.5.0 */

path(findButton, freeNarrator). /* 1.5 -> 1.6 */

path(freeNarrator, runWithNarrator). /* 1.6 -> 1.7 */
path(freeNarrator, attack). /* 1.6 -> 3.5 */
path(attack, successNeedWeapon). /* 3.5 -> 3.5.0 */
path(attack, _). /* 3.5 -> 3.6 TODO: FAIL and RESET */


path(ignore, class). /* 2 -> 3 */
path(ignore, toilet). /* 2 -> 2.0 */

path(toilet, class). /* 2.0 -> 3 */

path(class, seat). /* 3 -> 4 */
path(class, stand). /* 3 -> 3.0 */
path(class, askForSeat). /* 3 -> 3.1 */

path(askForSeat, stay). /* 3.1 -> 3.2 */
path(askForSeat, exitClass). /* 3.1 -> 3.1.0 */
path(stay, exitClass). /* 3.2 -> 3.1.0 */
path(stay, pressButton). /* 3.2 -> 3.3 */

path(exitClass, exitSchool). /* 3.1.0 -> 3.1.1 */
path(exitClass, talkToTeacher). /* 3.1.0 -> 5 */
path(exitSchool, _). /* 3.1.1 -> TODO: RESET */

path(seat, talkToTeacher). /* 4 -> 5 */
path(seat, exitClass). /* 4 -> 3.1.0 */

path(talkToTeacher, registerAtSchool). /* 5 -> 6 */
path(registerAtSchool, leave). /* 6 -> 7 TODO?? */
path(talkToTeacher, pressButton). /* 5 -> 3.3 */

path(pressButton, helpNarrator). /* 3.3 -> 3.4 */
path(pressButton, runWithOutNarrator). /* 3.3 -> 1.5.0 */

path(helpNarrator, attack). /* 3.4 -> 3.5*/
path(helpNarrator, runWithNarrator). /* 3.4 -> 1.7 */

/* These facts tell where the various objects in the game are located. */

at(useless_item, stay).

/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* lists the objects you are holding */

inventory :- holding(X), write(X), nl, fail.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */

right :- go(right01).
up_stairs :- go(stairs).
hallway :- go(hallway01).
slightly_opened_door :- go(slightlyOpenedDoor).
door :- go(door01).
panic :- go(panic).
teacher_1 :- go(teacher01).
teacher_2 :- go(teacher02).
office :- go(office).
exit :- go(exit).
ignore :- go(ignore).
class :- go(class).
toilet :- go(toilet).
coffee :- go(coffee).
seat :- go(seat).
stand :- go(stand).
stay :- go(stay).
ask_for_seat :- go(askForSeat).
explore_on_your_own :- go(exploreOnYourOwn).
hide_in_toilet :- go(hideInToilet).
go_to_light_saber :- go(goToLightSaber).
trust_narrator :- go(trustNarrator).
find_button :- go(findButton).
run_without_narrator :- go(runWithOutNarrator).
free_narrator :- go(freeNarrator).
attack :- go(attack).
success_need_weapon :- go(successNeedWeapon).
exit_class :- go(exitClass).
press_button :- go(pressButton).
talk_to_teacher :- go(talkToTeacher).
register_at_school :- go(registerAtSchool).
leave :- go(leave).
help_narrator :- go(helpNarrator).
run_with_narrator :- go(runWithNarrator).
exit_school :- go(exitSchool).

/* This rule tells how to move in a given direction. */

go(Destination) :-
        i_am_at(Here),
        path(Here, Destination),
        retract(i_am_at(Here)),
        assert(i_am_at(Destination)),
        !, look.

go(_) :-
        write('You can''t go that way.').


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


/* This rule tells how to die. */

die :- finish.


/* Get the user name */

get_name(john).
write_name :- get_name(X), write(X).

name :-
        write('please type your name: '),
        nl,
        read(X),
        retract(get_name(john)), /* TODO: fix, name can only be changed once */
        assert(get_name(X)),
        heisenberg(X).

heisenberg(Name) :- 
        Name = 'say_my_name', 
        write('Heisenberg'), 
        retract(get_name(Name)),
        assert(get_name('Heisenberg')).

/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.


/* This rule just writes out game instructions. */
/* TODO: Update */
instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(entrance) :- write('Start Text'), nl.
describe(stay) :- 
        /* todo: wait 1min? */
        write_name,
        write(' wasn''t sure if he should go up the stairs. He was thinking if it is the right decision to make, but after a good while he finally decided to go up the stairs.'),
        nl,
        /* todo: wait 1min? */
        write_name,
        write(', hello? Are you still here, you have to work with me already. I have to tell a story.'),
        nl,
        /* todo: wait 1min? */
        write_name,
        write(' was probably not there anymore, he just left the narrator before even making one decision. Oh wait you were looking around the whole time and just noticed a weird looking item'),
        /* todo: insert item */
        write('?'),
        nl,
        write('Do you want to pick it up?'),
        nl.