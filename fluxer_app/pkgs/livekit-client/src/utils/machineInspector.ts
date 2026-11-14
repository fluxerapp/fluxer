// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
export interface InspectableMachine {
	readonly id: string;
	readonly initialState: string;
	readonly states: Record<string, Record<string, unknown>>;
	readonly context?: unknown;
	currentState(): string;
	canHandle(input: string): boolean;
	on(eventName: string, callback: (data: any) => void): {off(): void};
}

export interface MachineAnnouncement {
	label: string;
	seq: number;
	at: number;
	machine: InspectableMachine;
}

const HISTORY_LIMIT = 16;

let enabled = false;
let announced: Array<MachineAnnouncement> = [];
let subscribers: Array<(announcement: MachineAnnouncement) => void> = [];

export function enableMachineInspector() {
	enabled = true;
}

export function isMachineInspectorEnabled() {
	return enabled;
}

export function announceMachine(label: string, machine: InspectableMachine) {
	if (!enabled) {
		return;
	}
	const announcement: MachineAnnouncement = {
		label,
		seq: announced.length,
		at: Date.now(),
		machine,
	};
	announced = [...announced.slice(-(HISTORY_LIMIT - 1)), announcement];
	for (const subscriber of subscribers) {
		subscriber(announcement);
	}
}

export function onMachineAnnounced(callback: (announcement: MachineAnnouncement) => void) {
	subscribers = [...subscribers, callback];
	for (const announcement of announced) {
		callback(announcement);
	}
	return () => {
		subscribers = subscribers.filter((subscriber) => subscriber !== callback);
	};
}
