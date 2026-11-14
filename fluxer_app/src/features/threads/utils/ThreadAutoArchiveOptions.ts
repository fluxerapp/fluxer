// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ONE_HOUR_DURATION_DESCRIPTOR as D_ONE_HOUR_DESCRIPTOR,
	ONE_WEEK_DURATION_DESCRIPTOR as D_ONE_WEEK_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import type {I18n} from '@lingui/core';

export function getAutoArchiveOptions(i18n: I18n): ReadonlyArray<RadioOption<number>> {
	return [
		{value: 60, name: i18n._(D_ONE_HOUR_DESCRIPTOR)},
		{value: 1440, name: i18n._(D.ONE_DAY_DESCRIPTOR)},
		{value: 4320, name: i18n._(D.THREE_DAYS_DESCRIPTOR)},
		{value: 10080, name: i18n._(D_ONE_WEEK_DESCRIPTOR)},
	];
}
