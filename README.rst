xbm-life
=========

.. image:: https://raw.github.com/wasamasa/xbm-life/master/img/screencast.gif

About
-----

An implementation of `Conway's Game of Life
<https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life>`_ using Emacs'
support for XBM images.  Requires a graphical Emacs instance to run.

Installation
------------

Install via `quelpa <https://github.com/quelpa/quelpa>`_ with ``M-:
(quelpa '(xbm-life :fetcher github :repo "wasamasa/xbm-life"))``.
Alternatively download the ``xbm-life.el`` file, open it in Emacs and
execute ``M-x eval-buffer``.

Usage
-----

Run ``M-x xbm-life`` for the demo.

========================= ================================
Key bind                  Function
========================= ================================
``l``                     Load pattern
``L``                     Load random pattern
``r``                     Reset
``R``                     Randomize
``p``, ``SPC``            Toggle play/pause
``.``                     Advance by a single generation
``+``, ``-``              Speed up/down
``M-+``, ``M--``          Make tiles larger/smaller
``C-+``, ``C--``          Make grid larger/smaller
``t``                     Toggle wraparound
``i``                     Invert colors
``<mouse-1>``             Toggle cell
``q``                     Bury buffer
========================= ================================

Contributing
------------

If you find bugs, have suggestions or any other problems, feel free to
report an issue on the issue tracker or hit me up on IRC, I'm always on
``#emacs``.  Patches are welcome, too, just fork, work on a separate
branch and open a pull request with it.
