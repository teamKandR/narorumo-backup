#!/usr/bin/env python3

from ltg_util import apply_card
from ltg_util import apply_slot
from ltg_util import copy
from ltg_util import smash
from ltg_util import apply_slot0_to_slot1

def build_abe(slot):
    """(S (K (S I I)))"""
    build_dianne(slot)
    apply_card("S", slot)

def build_clarice(slot):
    apply_slot(slot,"S")
    apply_card("K",slot)
    apply_card("S",slot)
    apply_slot(slot,"K")
    apply_card("S",slot) # and now the S around clarice.

def build_dianne(slot):
    apply_card("put",slot)
    apply_card("S",slot)
    apply_slot(slot,"I")
    apply_card("K",slot)

def build_fanny():
    """((S (K K)) (K K)).
    Uses slots 0 and 1, result ends up in slot 0."""
    apply_card("put", 1)
    apply_slot(1, "K")
    apply_card("K", 1)
    build_greg(0)
    apply_slot0_to_slot1()

def build_greg(slot):
    """(S (K K))"""
    apply_card("put", slot)
    apply_slot(slot, "K")
    apply_card("K", slot)
    apply_card("S", slot)

def build_horace(slot):
    """(S (K S))"""
    apply_card("put", slot)
    apply_slot(slot, "S")
    apply_card("K", slot)
    apply_card("S", slot)

def build_ian(slot):
    """(K I)"""
    apply_card("put", slot)
    apply_card("K", slot)

def build_june1():
    """june1: takes up slots 0 and 1.
    (S (horace (greg (K S))))"""
    # put greg in 0
    build_greg(0)
    # put S in 1
    apply_card("put", 1)
    apply_slot(1, "S")
    # put K around the S
    apply_card("K", 1)
    # smash together to get (greg (K S))
    smash()
    # copy (greg (K S)) to 1
    copy(0, 1)
    # put horace in 0
    build_horace(0)
    # smash together for (horace (greg (K S)))
    smash()
    # wrap with an S for (S (horace (greg (K S))))
    apply_card("S", 0)

def build_june2():
    """june2: takes up slots 0 and 1.
    ((S (horace fanny)) (greg I)) """
    # build fanny (always in 0)
    build_fanny()
    # move fanny to 1. 
    copy(0,1)
    # build horace in 0
    build_horace(0)
    # smash together to get (horace fanny) in 0
    smash()
    # wrap with an S
    apply_card("S", 0)
    # build greg in slot 1
    build_greg(1)
    # apply greg to I
    apply_slot(1, "I")
    # smash together to get ((S (horace fanny)) (greg I))
    smash()

def build_june():
    """
    (S (horace ((S (horace (greg (K S)))) ((S (horace fanny)) (greg I)))))
    """
    # build june2 in slot 0.
    build_june2()
    # copy june2 to slot 2.
    copy(0, 2)
    # build june1 in slots 0,1.
    build_june1()
    # copy june2 back to slot 1.
    copy(2,1)
    # smash together to get (june1 june2) in slot 0.
    smash()
    # copy (june1 june2) to slot 1.
    copy(0, 1)
    # build horace in slot 0
    build_horace(0)
    # smash together to get (horace june1 june2) in slot 0.
    smash()
    # wrap with an S for (S (horace june1 june2))
    apply_card("S", 0)

def build_kelly():
    """((S (horace fanny)) ian)"""
    # build fanny (always in 0)
    build_fanny()
    # build horace in 2.
    build_horace(2)

    # move fanny to 1. 
    apply_card("put", 1)
    apply_slot(1, "zero")
    apply_card("get", 1)

    # move horace to 0.
    apply_card("put", 0)
    apply_slot(0, "zero")
    apply_card("succ", 0)
    apply_card("succ", 0)
    apply_card("get", 0)

    # smash together to get (horace fanny) in 0.
    apply_slot0_to_slot1()

    # put an S around (horace fanny) in 0 to get (S (horace fanny))
    apply_card("S", 0)

    # build ian in 1.
    build_ian(1)
    # smash together to get ((S (horace fanny)) ian) in 0.
    apply_slot0_to_slot1()

