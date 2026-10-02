// SPDX-License-Identifier: AGPL-3.0-or-later

const FULL_KEYBOARD_MASK = 0xfffffffe;

export interface LinuxEvdevAccessProbe {
	totalEventDevices: number;
	readableEventDevices: number;
	keyboardDevices: number;
	readableKeyboardDevices: number;
	inInputGroup: boolean;
}

export function isFullKeyboardKeyCapabilities(keyCapabilities: string): boolean {
	const words = keyCapabilities.trim().split(/\s+/);
	const lowestWord = words[words.length - 1] ?? '';
	const lowestBits = Number.parseInt(lowestWord.slice(-8), 16);
	if (!Number.isFinite(lowestBits)) return false;
	return (lowestBits & FULL_KEYBOARD_MASK) >>> 0 === FULL_KEYBOARD_MASK;
}

export function hasLinuxEvdevAccess(probe: LinuxEvdevAccessProbe): boolean {
	if (probe.inInputGroup) return true;
	if (probe.keyboardDevices > 0) return probe.readableKeyboardDevices === probe.keyboardDevices;
	return probe.readableEventDevices > 0;
}
