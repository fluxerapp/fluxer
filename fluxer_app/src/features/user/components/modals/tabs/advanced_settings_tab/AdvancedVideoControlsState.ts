// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const ENCODER_PATH_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Encoder preference for new screen shares.',
	comment: 'Description for the encoder path select.',
});
const ENCODER_PATH_NO_HARDWARE_DESCRIPTOR = msg({
	message: 'No hardware encoder was found on this device, so Prefer hardware works like Automatic.',
	comment:
		'Description for the encoder path select, shown when this device has no hardware video encoder. Prefer hardware and Automatic are the option labels of the same select.',
});

export function selectScreenShareEncoderPathDescription(hardwareUnavailable: boolean): MessageDescriptor {
	return hardwareUnavailable ? ENCODER_PATH_NO_HARDWARE_DESCRIPTOR : ENCODER_PATH_DESCRIPTION_DESCRIPTOR;
}
