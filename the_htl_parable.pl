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

path(panic, reset01). /* 0.3.1 -> 0.3.2 */

path(stairs, teacher02). /* 1 -> 1.0 */
path(stairs, ignore). /* 1 -> 2 */

path(teacher02, office). /* 1.0 -> 1.1 */

path(office, coffee). /* 1.1 -> 1.2 */
path(office, class). /* 1.1 -> 3 */

path(coffee, reset02). /* 1.2 -> 1.2.0 */
path(coffe, exit). /* 1.2 -> 1.3 */

path(exit, exploreOnYourOwn). /* 1.3 -> 1.3.0 */
path(exploreOnYourOwn, hideInToilet). /* 1.3.0 -> 1.3.0.0 */
path(hideInToilet, reset03). /* 1.3.0.0 -> 1.3.4 */ 
path(exploreOnYourOwn, goToLightSaber). /* 1.3.0 -> 1.3.1 */ 
path(goToLightSaber, reset03). /* 1.3.1 -> 1.3.4 */
path(exit, trustNarrator). /* 1.3 -> 1.4 */

path(trustNarrator, hideInToilet). /* 1.4 -> 1.3.0.0 */
path(trustNarrator, findButton). /* 1.4 -> 1.5 */

path(findButton, runWithOutNarrator).  /* 1.5 -> 1.5.0 */

path(findButton, freeNarrator). /* 1.5 -> 1.6 */

path(freeNarrator, runWithNarrator). /* 1.6 -> 1.7 */
path(freeNarrator, attack). /* 1.6 -> 3.5 */
path(attack, successNeedWeapon). /* 3.5 -> 3.5.0 */
path(attack, reset04). /* 3.5 -> 3.6 */

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
path(exitSchool, reset05). /* 3.1.1 -> 3.1.2 */

path(seat, talkToTeacher). /* 4 -> 5 */
path(seat, exitClass). /* 4 -> 3.1.0 */

path(talkToTeacher, registerAtSchool). /* 5 -> 6 */
path(registerAtSchool, leave). /* 6 -> 7 TODO?? */
path(talkToTeacher, pressButton). /* 5 -> 3.3 */

path(pressButton, helpNarrator). /* 3.3 -> 3.4 */
path(pressButton, runWithOutNarrator). /* 3.3 -> 1.5.0 */

path(helpNarrator, attack). /* 3.4 -> 3.5*/
path(helpNarrator, runWithNarrator). /* 3.4 -> 1.7 */

path(reset01, entrance). /* 0.3.2 -> 0 */
path(reset02, entrance). /* 1.2.0 -> 0 */
path(reset03, entrance). /* 1.3.4 -> 0 */
path(reset04, entrance). /* 3.6 -> 0 */
path(reset05, entrance). /* 3.1.2 -> 0 */     
path(leave, entrance). /* 7 -> 0 */
path(runWithNarrator, entrance). /* 1.7 -> 0 */
path(runWithOutNarrator, entrance). /* 1.5.0 -> 0 */

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
panic :- go(panic), go(reset01).
teacher_1 :- go(teacher01).
teacher_2 :- go(teacher02).
office :- go(office).
exit :- go(exit).
ignore :- go(ignore).
class :- go(class).
toilet :- go(toilet).
coffee :- go(coffee), go(reset02).
seat :- go(seat).
stand :- go(stand).
stay :- go(stay).
ask_for_seat :- go(askForSeat).
explore_on_your_own :- go(exploreOnYourOwn).
hide_in_toilet :- go(hideInToilet), go(reset03).
go_to_light_saber :- go(goToLightSaber), go(reset03).
trust_narrator :- go(trustNarrator).
find_button :- go(findButton), go(runWithOutNarrator).
free_narrator :- go(freeNarrator), go(runWithNarrator).
attack :- go(attack), go(reset04).
success_need_weapon :- go(successNeedWeapon).
exit_class :- go(exitClass).
press_button :- go(pressButton), go(runWithOutNarrator).
talk_to_teacher :- go(talkToTeacher).
register_at_school :- go(registerAtSchool), go(leave).
leave :- go(leave).
help_narrator :- go(helpNarrator), go(runWithNarrator).
exit_school :- go(exitSchool), go(reset05).

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

/* 0 */
describe(entrance) :- 
        name,
        write('Narrator: '),
        write_name,
        write(' just entered the HTL, you are wondering where you are and what this strange building is. Looking to the right you see a hallway and in front of you are stairs. After a little while you decide to go up the stairs.'),
        nl.

/* 0.1 */
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

/* 0.2 */
describe(right01) :-
        write_name,
        write(' went right and not up the really nice-looking stairs, which is ok, he probably just wanted to explore a little, so he admires them and walks away.'),
        nl.

/* 0.2.0 */
describe(slightlyOpenedDoor) :-
        write_name,
        write(' persisted to not follow any instructions, whatsoever and just goes into the open door.'),
        nl,
        write_name,
        write(' don''t you know that''s a little bald of you.'),
        nl,
        write('Anyway, you look around for a bit and find a red triangle, '),
        write_name,
        write(' ignores it and goes back to the stairs he wanted to go up.'),
        /* TODO: item */
        nl.

/* 0.3 */
describe(hallway01) :-
        write_name,
        write(' wanted to prove that he is in control of the story, so he went down the hallway instead of going the intended way.'),
        nl,
        write('While walking '),
        write_name,
        write(' noticed a teacher in front of him. He thinks that it is not too late to get back to the main story and talked to him.'),
        nl.

/* 0.3.0 */
describe(teacher01) :-
        write_name,
        write(' finally did the right thing and talked to the teacher. He doesn''t even know why because he didn''t have anything to talk about. He just rambled a bit about the weather. '),
        write_name,
        write(' didn''t get why but the conversation flow really well and the teacher invited him to follow him into an office which '),
        write_name,
        write(' gladly accepted.'),
        nl.

/* 0.3.1 */
describe(panic) :-
        write('But no. Of course, not because '),
        write_name,
        write(' never does what he is supposed to do. So, he just started to panic he didn''t know what to say so he just said an unbelievably stupid excuse and ran off.'),
        nl.

/* 0.4 */
describe(door01) :-
        write('But of course, '),
        write_name,
        write(' didn''t just talk to the teacher because '),
        write_name,
        write(' has no social skills what so ever so he just entered to door to his left. He quickly closed the door behind him. When he looked up, he found a huge dark hole in front of him. He was really freaked out by It and didn''t really know what to do. He thought if he should just go out of the school as quickly as possible, but no, he is too much into it now so he just jumped into the hole.'),
        nl.

/* 1 */
describe(stairs) :-
        write('You go up the stairs and think “what a nice place”. In front of you is a teacher. You are wondering if you should talk to him, but you decide not to and just walk by since you wouldn''t have anything to talk about anyway. as you walk up the stairs you notice a knife hanging on the wall, you probably shouldn''t take things that don''t belong to him.'),
        /* TODO: item */
        nl.

/* 1.0 */
describe(teacher02) :-
        write_name,
        write(' just greeted the teacher. He didn''t even know why but he just kept saying words and it kept working. It seemed to be an really interesting conversation. After like 5 minutes of talk the teacher invited him to his office but '),
        write_name,
        write(' didn''t except his offer because it just didn''t feel right.'),
        nl.

/* 1.1 */
describe(office) :-
        write_name,
        write(' just ignored the story path because he wanted to “explore on his own” or something like that so he is just following the teacher into the office. So, the conversation starts going again and the teacher offers him a cup of coffee. He realises that he is not on the right way and quickly goes back to the classroom and refuses the coffee.'),
        nl.

/* 1.2 */
describe(coffee) :-
        write('You know you shouldn''t drink something from strangers, I really tried to keep you out of this. So as '),
        write_name,
        write(' drinks the coffer he starts to feel a little dizzy and nauseous, and then suddenly blacks out. When he opens his eyes again, he is tied to a chair in a dark room that looks to be a basement.'),
        /* Todo: use item #2 */
        nl.

/* 1.3 */
describe(exit) :-
        write_name,
        write(' uses the knife that he picked up earlier and frees himself running out on the hallway, I know '),
        write_name,
        write(' we had a rough start but this is a really bad situation, in order for you to be save you need to trust me now, and do exactly what I say.'),
        nl.

/* 1.3.0.0 */
describe(toilet) :-
        write_name,
        write(' quickly runs into the toilet and tries to his, sadly the teacher saw him, follows him into the toilet and gives him a Frühwarnung.'),
        nl.

describe(ignore) :-
        nl.

describe(class) :-
        nl.

describe(seat) :-
        nl.

describe(stand) :-
        nl.

describe(askForSeat) :-
        nl.

/*  
TODO: 0.3.2 - wann wird es getriggerd? 
TODO: 1.3.0.0 - explore
TODO: 1.2.0 - wann triggern wenn es kein sleep gibt?
*/

/* RESETS */

/* 0.3.2 */
describe(reset01) :-
        write(' Oh'), 
        write_name, 
        write('this is a mess, this is really a mess, I don''t even know where you are right now. Let me look through the script quickly *scrolling through pages* no, no, this isn''t supposed to happen. You have run so far off it isn''t ever worth the effort trying to save this. Let me just reset the game and you can try again. '),
        nl,
        go(entrance), 
        retractall(inventory).

/* 1.2.0 */
describe(reset02) :-
        write(' You just decide to wait it out, what''s the worst that can happen, after about 10 minutes pass by, a teacher walks in. Oh '),
        write_name,
        write('  this is a mess, I don''t even know if this is in the script anymore, wait a sec. Let me find out what your options are *scrolls through papers* oh no '),
        write_name,
        write(' this doesn''t look good you are way of the intended path, no point in trying to get you back, let me just restart the game, it will be easier for us both this way. '),
        nl,
        go(entrance),
        retractall(inventory).

/* 1.3.4 */
describe(reset03) :-
        write(' Well, that didn''t go well for you, maybe you''ll do better in the next run '),
        nl,
        go(entrance),
        retractall(inventory).

/* 1.5.0 */
describe(runWithOutNarrator) :- 
        write(' You coward, you just ran off without me, well I can''t be free neither can you, I''ll reset the game and we will start all over again. '),
        nl,
        go(entrance),
        retractall(inventory).

/* 3.1.2 */
describe(reset05) :- 
        write('  Let me just reset you back to the beginning. Don''t ever do that again. '), 
        nl, 
        go(entrance),
        retractall(inventory).

/* 3.6 */
describe(reset05) :-
        write(' Sadly, you are very bad at LOAL and your answer was wrong so you get the Frühwarnung and I have to reset the game :( '),
        nl,
        go(entrance),
        retractall(inventory).

/* 7 */
describe(leave) :- 
        write(' He left the school happy with his decision.'),
        nl,
        go(entrance),
        retractall(inventory).

/* 1.7 */
describe(runWithNarrator) :-
        write('So, we ran off together and we finally escaped the HTL-Parable, yay happy end.'),
        nl, 
        go(entrance), 
        retractall(inventory).