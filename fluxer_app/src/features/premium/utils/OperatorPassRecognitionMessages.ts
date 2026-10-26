// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const OPERATOR_PASS_RECOGNITION_DESCRIPTOR = msg({
	message:
		'Includes valid passes you have and passes you bought and passed on. Keep at least one active pass to retain your badge and wall listing. Paused or ended passes do not count.',
	comment:
		'Explains the public Operator Pass count. Each valid pass counts once per account, including purchases passed on. Holding an active pass is still required for benefits.',
});

export const OPERATOR_PASS_RECOGNITION_COUNT_DESCRIPTOR = msg({
	message: 'Badge and wall count: {count}',
	comment:
		'Public Operator Pass count in private settings, separate from current holdings. {count} is a number, including valid purchases passed on.',
});
