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


def print_comps(comps):
    """Aux. function to print models nicely"""
    for fields in comps:
        out = []
        for field in fields:
            formatted = format_quantum_numbers(field)
            out.append(formatted)

        print(" + ".join(out))


def get_comps(key, dict_):
    """Returns completions of operator labelled `key` as a dictionary mapping fields
    (represented as tuples of quantum numbers) to a list of completion objects.
    If `key` is a list it returns the completions of all operators in the list.

    """
    if isinstance(key, str):
        val = dict_[key]
        eff = EffectiveOperator(key, val)
        print(f"Finding completions of {key}")
        comps = collect_completions(clean_completions(operator_completions(eff)))
        return comps

    if isinstance(key, list):
        if len(key) == 1:
            return get_comps(key[0], dict_)

        return {**get_comps(key[0], dict_), **get_comps(key[1:], dict_)}

    raise ValueError("Key not a string or a list of strings!")


def generate_d6_ops():
    """Generates dimension-6 operators for filtering"""
    ops = sm_singlets(L, L.conj, Q, Q.conj)
    return {"O_{lq}^" + k: v for k, v in zip(string.ascii_letters, ops)}


def generate_d8_ops():
    """Generates dimension-8 operators"""
    ops = sm_singlets(L, L.conj, Q, Q.conj, H, H.conj)
    return {"O_{lq8}^" + k: v for k, v in zip(string.ascii_letters, ops)}


def print_report():
    """Generates and prints operators and completions. For use in a Jupyter
    notebook.

    """
    from IPython.display import display, Math

    print("Dimension 6:")
    print("============")
    d6_ops = generate_d6_ops()
    for label, operator in d6_ops.items():
        display(Math(label))
        display(Math(operator.latex()))

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
        display(Math(label))
        display(Math(operator.latex()))

    # Again in this case we want to remove the $k$ operator
    wanted_comps = ["O_{lq8}^" + char for char in string.ascii_lowercase]
    wanted_comps.remove("O_{lq8}^k")

    d8_comps = get_comps(wanted_comps, d8_ops)
    filtered = filter_completions(d8_comps, d6_comps)

    print_comps(filtered)
