Hello, and welcome to the Kippod project! We are very glad that you're considering to contribute to this project. With your help, we will be even more likely to achieve our common goals!

Following these guidelines will help the project managers and lead developers understand your contribution and will show them your respect for their time in managing this project. In return, they should reciprocate that respect when addressing your issue, assessing your contribution and aiding you in finalizing your pull requests.

# Project Goal
Our goal is to create an open-source, developer-friendly software for the simulation of nuclear reactors based on a diffusion approximation. These codes most often come in a package of a neutron transport code that supplies homogenized cross sections and a diffusion code. We only concern ourselves with the second part in this project.

Our vision is that best practices will be shared across the academic community, and that this project might someday be used as part of a regulatory review of nuclear reactor safety.

# Preferred contributions
As an open-source project, we are always excited for contributions from our community (which now includes you!).
This project is currently in its infancy, so we are looking for many different kinds of contributions:
1. Nuclear engineers with modern FORTRAN experience are needed in every source code file right now.
2. Developers with modern FORTRAN experience are welcome in every module that isn't nuclear-specific. For example, the utils package, the IO package and the like. For now just avoid the struct package.
3. If you're lacking in modern FORTRAN experience but looking for a place to start, you've come to the right place! We will try our best to help you learn the things you need to know, but in the meantime, there is still much more work to do that doesn't require coding experience. While you're learning, you can read existing software, and add comments and documentation.
4. If you're not interested in coding but still willing to contribute, one of our goals is to improve our overall documentation. I mean, look at this file. This is a mess. We need you!

## Things we are not looking for
1. Suggestions to use or actual use of existing export-controlled or proprietary software/information as part of the project.
2. Ideas for future features.

# Social Contract
Like with any group of people, there is an implied and an explicit social contract when dealing with each other. We believe in being as explicit as possible in this community, especially since interactions can happen without in-person contact. We have a code of conduct file, go read that. It is based on a very common code of conduct, and we will update it with additional expectations as we go along.
In addition to that most-important requirement of all people to be respectful and welcoming to others, we have some code-specific requirements:
* All code on master must compile when invoking make on the src directory.
* All code on master must pass all existing unit tests and regression tests.
* Don't add dependencies on third-party libraries without explicit project management permission.
* Keep updates as small as possible between commits. It makes reviewing things so much easier.
* Commit messages must be short and concise. Long commits are more difficult to use. Only use those for major changes (at which point you're violating the last item already!). Try to fit them within a standard 140 character tweet limit. :)
* Create issues for changes you make before you make them. This allows the community to give feedback and for project management to ask you for specific things that will ease the act of finalizing your pull requests later.
* Label your issues. When we try to solve issues, we have a state of mind, and we try to match our state of mind to what we do.
* Source code is required to follow the common format. Right now we don't have a file that explains that format, but once it is up, we expect any new source code to be indistinguishable in format. This allows for easier development later.

# Where to begin?
Still not sure where to begin? Why not start by looking at the issues? We will try to mark issues for beginners, as well as non-nuclear-specific issues that are easier to approach without any understanding of what a nuclear reactor is.
Help-wanted issues for things we never get to will also be flagged. Any contribution there will grant you our gratitude. It is well known that our gratitude is usually accompanied by gifts of icecream. Mmmmmm... Icecream...

## But how do I approach an open-source project?
Oh, you're new to this too! So were we when we started! As we gain more insight into these tools we will put a list of our favorite go-to sites to learn how to do these things better.

# Quick walkthrough
Currently we're unstructued in ways to commit, and commits to master are allowed directly. As the code takes shape we will move on to a fork-merge structure where each contribution that is more than 1-2 lines of code or that changes functionality in ANY WAY will require a fork.

For now, freestyle.

# Security issues
If you see any security issue with this code, please report them immediately to the project lead. DO NOT PUT THEM IN THE ISSUE TRACKER. We don't want people to use that volnurability until it is fixed! This software will hopefully be used one day in nuclear reactor safety calculations. The implications of malicious code here can be devastating!

# Code Review Process
Contributions are currently by approved contributers only. If you wish to join the project, feel free to email the project lead. Access will be granted unless the application itself violates the code of conduct.
We intend to move on to a more structured review system later, after the code takes shape.

# Current contributors:

Project lead: Eshed Magali, esheder@gmail.com
