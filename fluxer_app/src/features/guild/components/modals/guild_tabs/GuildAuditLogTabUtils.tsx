// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import type {User} from '@app/features/user/models/User';
import {getFormattedDateTime} from '@app/features/user/utils/DateFormatting';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';

export const formatTimestamp = (logId: string): string => {
	const timestamp = SnowflakeUtils.extractTimestamp(logId);
	return getFormattedDateTime(timestamp);
};

export interface AuditLogUserOption extends ComboboxOption<string> {
	user: User;
}

export const buildUserOptions = (members: Array<{user: User}> | undefined): Array<AuditLogUserOption> => {
	if (!members) return [];
	return members
		.slice()
		.sort((a, b) => NicknameUtils.getDisplayName(a.user).localeCompare(NicknameUtils.getDisplayName(b.user)))
		.map((member) => {
			const label = NicknameUtils.getDisplayName(member.user);
			return {value: member.user.id, label, user: member.user};
		});
};
