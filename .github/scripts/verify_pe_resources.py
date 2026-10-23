#!/usr/bin/env python3
# -*- Mode: Python; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:set ts=4 sw=4 sts=4 et:
"""Verify the resources embedded in a Windows executable of Flint.

This checks that flint.exe carries the manifest which opts into high DPI
support, and the icon of the application. It runs on any platform thanks to
pefile, so that the artifact built by CI can be examined without Windows:

    python3 -m pip install pefile
    python3 .github/scripts/verify_pe_resources.py path/to/flint.exe
"""

import sys
import xml.etree.ElementTree as ET

try:
    import pefile
except ImportError:
    sys.exit("error: pefile is not installed; try: python3 -m pip install pefile")

RT_ICON = 3
RT_GROUP_ICON = 14
RT_MANIFEST = 24
CREATEPROCESS_MANIFEST_RESOURCE_ID = 1

ICON_NAME = "flint"
COMMON_CONTROLS = "microsoft.windows.common-controls"

failures = []


def fail(message):
    failures.append(message)


def resource_name(entry):
    """Return the name of a resource directory entry, numeric or string."""
    if entry.name is not None:
        return entry.name.string.decode("utf-8", "replace")
    return entry.id


def same_name(a, b):
    """Compare resource names; Windows looks them up case-insensitively."""
    return str(a).casefold() == str(b).casefold()


def iter_resources(pe, type_id):
    """Yield (name, language, bytes) of the resources of the given type."""
    root = getattr(pe, "DIRECTORY_ENTRY_RESOURCE", None)
    if root is None:
        return
    for type_entry in root.entries:
        if type_entry.id != type_id:
            continue
        for name_entry in type_entry.directory.entries:
            name = resource_name(name_entry)
            for lang_entry in name_entry.directory.entries:
                data = lang_entry.data.struct
                yield (name,
                       lang_entry.id,
                       pe.get_data(data.OffsetToData, data.Size))


def elements_with_local_name(root, local_name):
    return [e for e in root.iter()
            if e.tag.rsplit("}", 1)[-1] == local_name]


def text_of(element):
    return (element.text or "").strip()


def check_manifest(pe):
    manifests = [(name, language, blob)
                 for name, language, blob in iter_resources(pe, RT_MANIFEST)
                 if name == CREATEPROCESS_MANIFEST_RESOURCE_ID]
    if not manifests:
        fail("no manifest (RT_MANIFEST #%d) is embedded"
             % CREATEPROCESS_MANIFEST_RESOURCE_ID)
        return
    if len(manifests) > 1:
        fail("%d manifests are embedded; exactly one is expected"
             % len(manifests))
    _, _, blob = manifests[0]
    if blob.startswith(b"\xff\xfe") or blob.startswith(b"\xfe\xff"):
        text = blob.decode("utf-16")
    else:
        text = blob.decode("utf-8-sig")

    print("manifest:")
    print(text)

    try:
        root = ET.fromstring(text)
    except ET.ParseError as e:
        fail("the embedded manifest is not well-formed XML: %s" % e)
        return

    awareness = elements_with_local_name(root, "dpiAwareness")
    if not awareness:
        fail("the manifest does not declare dpiAwareness")
    elif not text_of(awareness[0]).casefold().startswith("permonitorv2"):
        fail("the manifest does not declare PerMonitorV2 for dpiAwareness, "
             "but %r" % text_of(awareness[0]))

    aware = elements_with_local_name(root, "dpiAware")
    if not aware:
        fail("the manifest does not declare dpiAware for older versions of "
             "Windows")
    elif text_of(aware[0]).casefold() != "true":
        fail("the manifest declares %r for dpiAware, not true"
             % text_of(aware[0]))

    identities = [(e.get("name") or "").casefold()
                  for e in elements_with_local_name(root, "assemblyIdentity")]
    if COMMON_CONTROLS not in identities:
        fail("the manifest does not declare the dependency on Common Controls "
             "version 6")


def check_icon(pe):
    groups = [name for name, _, _ in iter_resources(pe, RT_GROUP_ICON)]
    if not groups:
        fail("no icon (RT_GROUP_ICON) is embedded")
    elif not any(same_name(name, ICON_NAME) for name in groups):
        fail("no icon named %r is embedded; found %s"
             % (ICON_NAME, ", ".join(str(g) for g in groups)))

    images = [name for name, _, _ in iter_resources(pe, RT_ICON)]
    if not images:
        fail("no image (RT_ICON) is embedded")


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: %s path/to/flint.exe" % sys.argv[0])

    try:
        pe = pefile.PE(sys.argv[1])
    except OSError as e:
        sys.exit("error: cannot read %s: %s" % (sys.argv[1], e))
    except pefile.PEFormatError as e:
        sys.exit("error: %s is not a PE file: %s" % (sys.argv[1], e))

    check_manifest(pe)
    check_icon(pe)

    if failures:
        for message in failures:
            print("error: " + message, file=sys.stderr)
        return 1
    print("ok: the manifest and the icon are embedded as expected")
    return 0


if __name__ == "__main__":
    sys.exit(main())
