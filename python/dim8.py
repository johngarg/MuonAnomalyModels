#!/usr/bin/env python3

import string

from neutrinomass.tensormethod import invariants, colour_singlets
from neutrinomass.tensormethod.sm import L, H, Q

from neutrinomass.completions import operator_completions
from neutrinomass.completions import clean_completions
from neutrinomass.completions import filter_completions
from neutrinomass.completions import collect_completions
from neutrinomass.completions import EffectiveOperator

from neutrinomass.completions.core import format_quantum_numbers


def sm_singlets(*fields):
    """Aux. function to generate SM invariants"""
    su2_singlets = invariants(*fields, ignore=("u", "d", "c"))
    return colour_singlets(su2_singlets, overcomplete=True)


def exotic_info(comp):
    """Returns exotic quantum numbers. If `pos_y` is True, returns representation of
    fields such that all hypercharges are non-negative.

    """
    info = {}
    for e in comp.exotics:
        # normalise hypercharge to be positive
        if e.is_scalar and e.y < 0:
            charges = sorted(e.conj.charges.items())
            sm = e.conj.sm_irrep
        else:
            charges = sorted(e.charges.items())
            sm = e.sm_irrep

        if e.is_fermion:
            lorentz = "F"
        elif e.is_scalar:
            lorentz = "S"
        else:
            raise ValueError("Unrecognised exotic field type.")

        info[e] = (lorentz,) + sm + tuple(charges)

    return info


def print_comps(comp_dict):
    """Aux. function to print models nicely"""
    for fields, comps in comp_dict.items():
        # They should all have the same field content, so just take first
        comp = comps[0]
        # Working with only left-handed fermion fields in Haskell code, so fix
        # that up here for comparison
        fermion_partners = [
            f.conj for f in comp.exotics if (f.is_fermion and not f.is_real_sm_irrep)
        ]
        comp.exotics = comp.exotics.union(set(fermion_partners))

        out = {format_quantum_numbers(f) for f in exotic_info(comp).values()}
        # Wrap in quotes for reading into Haskell
        out = [f'"{s}"' for s in out]
        out_joined = ", ".join(out)
        print(f"[{out_joined}]")


def get_comps(key, dict_, verbose=True):
    """Returns completions of operator labelled `key` as a dictionary mapping fields
    (represented as tuples of quantum numbers) to a list of completion objects.
    If `key` is a list it returns the completions of all operators in the list.

    """
    if isinstance(key, str):
        val = dict_[key]
        eff = EffectiveOperator(key, val)
        if verbose:
            print(f"Finding completions of {key}")
        comps = collect_completions(clean_completions(operator_completions(eff)))
        return comps

    if isinstance(key, list):
        if len(key) == 1:
            return get_comps(key[0], dict_, verbose=verbose)

        return {
            **get_comps(key[0], dict_, verbose=verbose),
            **get_comps(key[1:], dict_, verbose=verbose),
        }

    raise ValueError("Key not a string or a list of strings!")


def generate_d6_ops():
    """Generates dimension-6 operators for filtering"""
    ops = sm_singlets(L, L.conj, Q, Q.conj)
    return {"O_{lq}^" + k: v for k, v in zip(string.ascii_letters, ops)}


def generate_d8_ops():
    """Generates dimension-8 operators"""
    ops = sm_singlets(L, L.conj, Q, Q.conj, H, H.conj)
    return {"O_{lq8}^" + k: v for k, v in zip(string.ascii_letters, ops)}


def print_report(notebook=True):
    """Generates and prints operators and completions. For use in a Jupyter
    notebook.

    """
    from IPython.display import display, Math

    print("Dimension 6:")
    print("============")
    d6_ops = generate_d6_ops()
    for label, operator in d6_ops.items():
        if notebook:
            display(Math(label))
            display(Math(operator.latex()))
        else:
            print(label)
            print(operator)

    # Only $SU(2)$ structures $b$ and $c$ are interesting for the
    # neutral-current anomalies, since the first has an $SU(2)_L$ contraction
    # between the $L$ and $Q$ fields, leading to an operator without two muons
    # and two down-type quarks.
    d6_comps = get_comps(["O_{lq}^b", "O_{lq}^c"], d6_ops)
    print_comps(d6_comps)

    print("\nDimension 8:")
    print("============")
    d8_ops = generate_d8_ops()
    for label, operator in d8_ops.items():
        if notebook:
            display(Math(label))
            display(Math(operator.latex()))
        else:
            print(label)
            print(operator)

    # Again in this case we want to remove the $k$ operator
    wanted_comps = ["O_{lq8}^" + char for char in string.ascii_lowercase[:15]]
    wanted_comps.remove("O_{lq8}^k")

    d8_comps = get_comps(wanted_comps, d8_ops)
    filtered = filter_completions(d8_comps, d6_comps)

    print_comps(filtered)


def write_out():
    """Like `print_report` but just for writing the data out."""

    # Dimension 6
    d6_ops = generate_d6_ops()
    d6_comps = get_comps(["O_{lq}^b", "O_{lq}^c"], d6_ops, verbose=False)
    print_comps(d6_comps)

    # Dimension 8
    d8_ops = generate_d8_ops()
    wanted_comps = ["O_{lq8}^" + char for char in string.ascii_lowercase[:15]]
    wanted_comps.remove("O_{lq8}^k")

    d8_comps = get_comps(wanted_comps, d8_ops, verbose=False)
    filtered = filter_completions(d8_comps, d6_comps)

    print_comps(filtered)
