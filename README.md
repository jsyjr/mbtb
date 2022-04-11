# mbtb

mbtb -- Emacs Mini-Buffer frame overlaid on the Tab Bar (Proof of Concept)

## Commentary:

Mbtb is proof-of-concept for a minibuffer positioned over the menu-bar.
It is implemented using a separate mini-buffer frame per primary frame.

## Installation:

To try this package your default-frame-alist needs these entries:
```
   (minibuffer . nil)        ; supress minibuffer at bottom of frame
   (tab-bar-lines . 1)       ; space will be overlaid by the MBTB frame
```

You also need to enable a vaccuous version of tab-bar-mode enable:
```
  (setq tab-bar-format '((lambda () " ")))

  (tab-bar-mode  +1)
```

## Acknowledgement:

An earlier version of this package was far more complicated, less
performant and less robust.  Martin Rudalics repeatedly provided
great feedback.  The result is this much simpler package.
