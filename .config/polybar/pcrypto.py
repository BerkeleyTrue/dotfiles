#!/usr/bin/env python

import requests
import argparse
import json
import sys
import os

parser = argparse.ArgumentParser(description="Display currencies on polybar")
parser.add_argument("--coins", type=str,
                    nargs="+", help="Select coins to display")
parser.add_argument("--base", type=str,
                    nargs="?", default="USD", help="Currency base to convert against")
parser.add_argument("--decimals", type=int,
                    nargs="?", default=2, help="How many decimals to show")
parser.add_argument("--display", type=str,
                    nargs="?", default="price", choices=["price", "percentage", "both"], help="Display mode")

args = parser.parse_args()
home = os.path.expanduser("~/")

unicode_dict = {}
with open(f"{home}.config/polybar/coins.svg", "r", encoding="utf-8") as icons:
    for line in icons:
        unicode, coin = line.strip().split(":")
        unicode_dict[unicode] = coin


for coin in args.coins:
    get = requests.get(
        f"https://api.coinranking.com/v1/public/coins?prefix={coin}&base={args.base}").json()["data"]
    price_float = round(float(get["coins"][0]["price"]), args.decimals)
    current_price = get["base"]["sign"] + str(price_float)
    change = get["coins"][0]["change"]

    for _unicode, _coin in unicode_dict.items():
        if _coin == coin:
            icon = chr(int(_unicode, 16)) if len(_unicode) > 1 else _unicode
            if args.display == "price":
                sys.stdout.write(f" {icon} {current_price} ")
            if args.display == "percentage":
                sys.stdout.write(f"  {icon} {change:+}% ")
            if args.display == "both":
                sys.stdout.write(f" {icon} {current_price} | {change:+}% ")
