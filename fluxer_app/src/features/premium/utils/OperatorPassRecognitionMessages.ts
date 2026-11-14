// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';

export const OPERATOR_PASS_RECOGNITION_DESCRIPTOR = msg({
	message:
		"Your count includes the passes on your account and the passes you bought and passed on. Paused or ended passes don't count, and you need at least one pass on your account for the badge and the wall.",
	comment:
		"Explains the public Operator Pass count. Each valid pass counts once per account, including purchases passed on. The badge is the Operator badge and the wall is the Operators' wall.",
});

export const OPERATOR_PASS_RECOGNITION_COUNT_DESCRIPTOR = msg({
	message: 'Badge and wall count: {count}',
	comment:
		'Public Operator Pass count in private settings, separate from current holdings. {count} is a number, including valid purchases passed on.',
});
