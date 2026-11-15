// SPDX-License-Identifier: AGPL-3.0-or-later

import {VoiceMeshEngine, type VoiceMeshTarget} from '@app/features/voice/engine/mesh/VoiceMeshEngine';
import {Room, RoomEvent, type RoomOptions} from 'livekit-client';

const meshRooms = new WeakSet<Room>();

export function createVoiceMeshRoom(roomOptions: RoomOptions, target: VoiceMeshTarget): Room {
	const room = new Room({
		...roomOptions,
		dynacast: false,
		e2ee: undefined,
		encryption: undefined,
		publishDefaults: {...roomOptions.publishDefaults, simulcast: false, backupCodec: false},
		createEngine: (options) => new VoiceMeshEngine(options, target),
	});
	room.on(RoomEvent.Connected, () => {
		if (room.engine instanceof VoiceMeshEngine) room.engine.start(room);
	});
	meshRooms.add(room);
	return room;
}

export function isVoiceMeshRoom(room: Room): boolean {
	return meshRooms.has(room);
}

export function holdVoiceMeshConversion(room: Room | null, held: boolean): void {
	if (room?.engine instanceof VoiceMeshEngine) room.engine.holdConversion(held);
}
