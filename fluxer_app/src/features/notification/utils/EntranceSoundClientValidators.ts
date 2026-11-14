// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ENTRANCE_SOUND_EXT_TO_MIME,
	ENTRANCE_SOUND_EXTENSIONS,
	ENTRANCE_SOUND_MAX_BYTES,
	type EntranceSoundExtension,
} from '@fluxer/constants/src/EntranceSoundConstants';

const ENTRANCE_SOUND_FILE_EXTENSIONS: ReadonlyArray<string> = ENTRANCE_SOUND_EXTENSIONS.map((ext) => `.${ext}`);

const ENTRANCE_SOUND_MIME_TYPES: ReadonlyArray<string> = ENTRANCE_SOUND_EXTENSIONS.map(
	(ext) => ENTRANCE_SOUND_EXT_TO_MIME[ext as EntranceSoundExtension],
);

export const ENTRANCE_SOUND_FILE_PICKER_ACCEPT = [...ENTRANCE_SOUND_MIME_TYPES, ...ENTRANCE_SOUND_FILE_EXTENSIONS].join(
	',',
);

type EntranceSoundFileValidationFailure = 'too_large' | 'invalid_type';

export type EntranceSoundFileValidationResult =
	| {valid: true}
	| {valid: false; reason: EntranceSoundFileValidationFailure};

export function isValidEntranceSoundFile(file: File): EntranceSoundFileValidationResult {
	if (file.size > ENTRANCE_SOUND_MAX_BYTES) {
		return {valid: false, reason: 'too_large'};
	}
	const lastDot = file.name.lastIndexOf('.');
	const extension = lastDot >= 0 ? file.name.slice(lastDot).toLowerCase() : '';
	const extOk = ENTRANCE_SOUND_FILE_EXTENSIONS.includes(extension);
	const mimeOk = ENTRANCE_SOUND_MIME_TYPES.some((mime) => file.type === mime);
	if (!extOk && !mimeOk) {
		return {valid: false, reason: 'invalid_type'};
	}
	return {valid: true};
}
