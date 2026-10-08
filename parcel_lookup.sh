#!/usr/bin/env bash
# Usage: ./parcel_lookup.sh <commune> <parcel_number>
# Example: ./parcel_lookup.sh Aranno 357
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <commune> <parcel_number>" >&2
  exit 1
fi

commune="$1"
parcel="$2"

curl -s "https://api3.geo.admin.ch/rest/services/ech/SearchServer?searchText=${commune}+${parcel}&type=locations&origins=parcel" \
  | jq '.results[] | { x: .attrs.x, y: .attrs.y, detail: .attrs.detail }'
