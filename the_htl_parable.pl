/* the htl parable - by Leon Leeb, Paul Nell, Niklas Trinkl. */

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

path(askForSeat, wait). /* 3.1 -> 3.2 */
path(askForSeat, exitClass). /* 3.1 -> 3.1.0 */
path(wait, exitClass). /* 3.2 -> 3.1.0 */
path(wait, pressButton). /* 3.2 -> 3.3 */

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

path(reset0, entrance).

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
panic :- goNoMsg(panic).
teacher_1 :- go(teacher01).
teacher_2 :- go(teacher02).
office :- go(office).
exit :- go(exit).
ignore :- go(ignore).
class :- go(class).
toilet :- go(toilet).
coffee :- goNoMsg(coffee).
seat :- go(seat).
stand :- go(stand).
stay :- go(stay).
wait :- go(wait).
ask_for_seat :- go(askForSeat).
explore_on_your_own :- go(exploreOnYourOwn).
hide_in_toilet :- goNoMsg(hideInToilet).
go_to_light_saber :- goNoMsg(goToLightSaber).
trust_narrator :- go(trustNarrator).
find_button :- goNoMsg(findButton).
free_narrator :- goNoMsg(freeNarrator).
attack :- goNoMsg(attack).
exit_class :- go(exitClass).
press_button :- goNoMsg(pressButton).
talk_to_teacher :- go(talkToTeacher).
register_at_school :- goNoMsg(registerAtSchool).
leave :- go(leave).
help_narrator :- goNoMsg(helpNarrator).
exit_school :- goNoMsg(exitSchool).

/* This rule tells how to move in a given direction. */

go(Destination) :-
        i_am_at(Here),
        path(Here, Destination),
        retract(i_am_at(Here)),
        assert(i_am_at(Destination)),
        !, look.

go(_) :-
        write('You can''t go that way.').

goNoMsg(Destination) :-
        nl,
        describe(Destination);
        i_am_at(Here),
        path(Here, Destination),
        retract(i_am_at(Here)),
        assert(i_am_at(Destination)),
        fail.

/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl,
        available_paths(Place),
        nl.

/* Map for paths */

map(right, right01).
map(up_stairs, stairs).
map(hallway, hallway01).
map(slightly_opened_door, slightlyOpenedDoor).
map(door, door01).
map(panic, panic).
map(teacher_1, teacher01).
map(teacher_2, teacher02).
map(office, office).
map(exit, exit).
map(ignore, ignore).
map(class, class).
map(toilet, toilet).
map(coffee, coffee).
map(seat, seat).
map(stand, stand).
map(stay, stay).
map(wait, wait).
map(ask_for_seat, askForSeat).
map(explore_on_your_own, exploreOnYourOwn).
map(hide_in_toilet, hideInToilet).
map(go_to_light_saber, goToLightSaber).
map(trust_narrator, trustNarrator).
map(find_button, findButton).
map(free_narrator, freeNarrator).
map(attack, attack).
map(exit_class, exitClass).
map(press_button, pressButton).
map(talk_to_teacher, talkToTeacher).
map(register_at_school, registerAtSchool).
map(leave, leave).
map(help_narrator, helpNarrator).
map(exit_school, exitSchool).


available_paths(CurrentLocation) :-
    write('Available paths:'), nl,
    findall(Command, (map(Command, Destination), path(CurrentLocation, Destination)), Commands),
    print_available_commands(Commands).

print_available_commands([]).
print_available_commands([Command|Rest]) :-
    write(Command), nl,
    print_available_commands(Rest).


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        /* TODO: fix (wrong commands displayed) */
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

/* easter egg */
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
        write('start: Start the game.'), nl,
        write('name: Set your name.'), nl,
        write('inventory: Show which items you have.'), nl,
        write('look: Describe your sourrundings.'), nl,
        /* TODO: write('reset: Reset the game.'), nl,*/
        write('TODO'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look,
        nl.

/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

/* 0 */
describe(entrance) :- 
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
        write(' persisted to not follow any instructions, whatsoever and just goes into the open door. '),
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
        nl,
        goNoMsg(reset01).

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
        /* Todo: use item #2 & else reset */
        nl.

/* 1.3 */
describe(exit) :-
        write_name,
        write(' uses the knife that he picked up earlier and frees himself running out on the hallway, I know '),
        write_name,
        write(' we had a rough start but this is a really bad situation, in order for you to be save you need to trust me now, and do exactly what I say.'),
        nl.

/* 1.3.0 */
describe(exploreOnYourOwn) :-
        write('Oh come on, this is bad, this won''t go well for you, you really should have trusted me this time, anyway too late now, in front of you is a toilet door you could hide in, as you hear a teacher behind you, but in front of you is a lightsaber you could use to defend yourself, this is on you now, I am not even trying to get you back on the real path, you deicide.'),
        nl.

/* 1.3.0.0 */
describe(hideInToilet) :-
        write_name,
        write(' quickly runs into the toilet and tries to his, sadly the teacher saw him, follows him into the toilet and gives him a Frühwarnung.'),
        nl.

/* 1.3.1 */
describe(goToLightSaber) :-
        write('So '),
        write_name,
        write(' decided to sprint to the lightsaber, he grabbed it, and a teacher is approaching him with a deadly look in his eyes and a pistol that appears to fire Frühwarnungen whatever that means, he can block the first two shots and is ready for the next one, his inner jedi seems to be awaken but right when he wants to block the next one, a teacher from behind gives him the Frühwarnung.'),
        nl.

/* 1.4 */
describe(trustNarrator) :-
        write('Oh, that''s really great, it''s too late to get you on the intendent path so let''s try to escape together, go up the stairs again and into the classroom, it should be empty by now. When you are there, there is a button behind the second picture, press it.'),
        nl.

/* 1.5 */
describe(findButton) :-
        write('As you press the button a secret door opens behind you, you go in and see me, some behind me keeps me hostage but you decide to knock him out and help me.'),
        nl.

/* 1.6 */
describe(freeNarrator) :-
        write('So, you grabbed a conveniently placed pipe next to you and pet the guy behind me with it.'),
        nl.

/* 2 */
describe(ignore) :-
        write('As you walk by the teacher you hear him burbling about science in a classroom with a slightly open door. You don''t know the teacher, but you still enter the classroom because you are really interested in the topic.'),
        nl.

/* 2.0 */
describe(toilet) :-
        write('But no, '),
        write_name,
        write(' just decided to go into the toilet for no reason, '),
        write_name,
        write(' didn''t even need to pee now just stands there, wondering what he should do. No waiting any longer he decides to exit the toilet again and finally go into the classroom and listen to the teacher.'),
        nl.

/* 3 */
describe(class) :-
        write('The teacher sees you and asks you to take a seat. You think it is a bit odd, but more than welcoming and follow her instructions.'),
        nl.

/* 3.0 */
describe(stand) :-
        write_name,
        write(' weirdly enough didn''t follow the instructions but just stood there like a weirdo. What are you doing '),
        write_name,
        write('?'),
        nl.

/* 3.1 */
describe(askForSeat) :-
        write_name,
        write(' just ask someone to sit on their seat. *Quick interruption form narrator* hey, what are you doing, aren''t you ashamed man. So, as I was saying '),
        write_name,
        write(' asked to sit on the seat of someone else, probably because they were so confused and overwhelmed by the questions they said yes. So, he set on the seat and listened to the lesson of the teacher.'),
        nl.

/* 3.1.0 */
describe(exitClass) :-
        write('What are you doing '),
        write_name,
        write(' you should have talked to the teacher, whatever it''s not too late, just wait till they are done with the lesson and talk to them.'),
        nl.

/* 3.1.1 */
describe(exitSchool) :-
        write('So, you are just leaving? Are you serious? There are so many different paths you could have taken but you took this one? Whatever it''s too late now.'),
        nl.

/* 3.2 */
describe(wait) :-
        write_name,
        write(' just waited and looked around, he noticed a strange looking button on the wall and pressed it.'),
        nl.

/* 3.3 */
describe(pressButton) :-
        write('As you press the button a secret door opens behind you, you go in and see me, someone behind me keeps me hostage but you decide to knock him out and help me.'),
        nl.

/* 3.4 */
describe(helpNarrator) :-
        write('You grabbed a conveniently placed pipe next to you and pet the guy behind me with it.'),
        nl.

/* 3.5 */
describe(attack) :-
        write('Some other teacher throws a Loal question at you which you have to get right in order to doge the Frühwarnung.'),
        nl.

/* 4 */
describe(seat) :-
        write_name,
        write(' just took a seat and listened to the whole lesson, he thought it was very interesting. So, after the lesson he talked to the teacher.'),
        nl.

/* 5 */
describe(talkToTeacher) :-
        write('He walked up to the teacher and was like, “Hey man great lesson!” and they had a good conversation, after like 2 minutes of talking the teacher said that '),
        write_name,
        write(' should register at the school and that he''d really like it here which he did.'),
        nl.

/* 6 */
describe(registerAtSchool) :-
        write('So, '),
        write_name,
        write(' followed the instructions of the teacher and applied for the school.'),
        nl.


/* RESETS */

describe(reset0) :- 
        nl,
        write('Reset msg'),
        nl,
        goNoMsg(entrance),
        nl,
        /* TODO: retractall(inventory),*/
        available_paths(entrance),
        fail.

/* 0.3.2 */
describe(reset01) :-
        write('Oh '), 
        write_name, 
        write(' this is a mess, this is really a mess, I don''t even know where you are right now. Let me look through the script quickly *scrolling through pages* no, no, this isn''t supposed to happen. You have run so far off it isn''t ever worth the effort trying to save this. Let me just reset the game and you can try again.'),
        nl,
        goNoMsg(reset0);
        nl.

/* 1.2.0 */
describe(reset02) :-
        write('You just decide to wait it out, what''s the worst that can happen, after about 10 minutes pass by, a teacher walks in. Oh '),
        write_name,
        write(' this is a mess, I don''t even know if this is in the script anymore, wait a sec. Let me find out what your options are *scrolls through papers* oh no '),
        write_name,
        write(' this doesn''t look good you are way of the intended path, no point in trying to get you back, let me just restart the game, it will be easier for us both this way.'),
        nl,
        goNoMsg(reset0);
        nl.

/* 1.3.4 */
describe(reset03) :-
        write('Well, that didn''t go well for you, maybe you''ll do better in the next run.'),
        nl,
        goNoMsg(reset0);
        nl.

/* 1.5.0 */
describe(runWithOutNarrator) :- 
        write(' You coward, you just ran off without me, well I can''t be free neither can you, I''ll reset the game and we will start all over again. '),
        nl,
        goNoMsg(reset0);
        nl.

/* 3.1.2 */
describe(reset05) :- 
        write('  Let me just reset you back to the beginning. Don''t ever do that again. '), 
        nl, 
        goNoMsg(reset0);
        nl.

/* 3.6 */
describe(reset04) :-
        write('Sadly, you are very bad at LOAL and your answer was wrong so you get the Frühwarnung and I have to reset the game :/ '),
        nl,
        goNoMsg(reset0);
        nl.

/* 7 */
describe(leave) :- 
        write('He left the school happy with his decision.'),
        nl,
        goNoMsg(reset0);
        nl.

/* 1.7 */
describe(runWithNarrator) :-
        write('So, we ran off together and we finally escaped the HTL-Parable, yay happy end.'),
        nl, 
        goNoMsg(reset0);
        nl.