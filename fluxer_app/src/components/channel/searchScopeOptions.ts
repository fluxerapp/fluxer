/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {ChannelTypes} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {MessageSearchScope, SearchValueOption} from '~/utils/SearchUtils';

export interface ScopeValueOption extends SearchValueOption {
	value: MessageSearchScope;
}

export const DEFAULT_SCOPE_VALUE: MessageSearchScope = 'current';

export const getScopeOptionsForChannel = (i18n: I18n, channel?: ChannelRecord | null): Array<ScopeValueOption> => {
	const DM_SCOPE_OPTIONS: Array<ScopeValueOption> = [
		{
			value: 'current',
			label: i18n._(msg`Current DM`),
			isDefault: true,
			description: i18n._(msg`Search only in the current DM`),
		},
		{
			value: 'all_dms',
			label: i18n._(msg`All DMs`),
			description: i18n._(msg`Across all DMs you've ever been in`),
		},
		{
			value: 'open_dms',
			label: i18n._(msg`Open DMs`),
			description: i18n._(msg`Across all DMs you currently have open`),
		},
		{
			value: 'all',
			label: i18n._(msg`All DMs + Communities`),
			description: i18n._(msg`Across all DMs you've ever been in + all Communities you're currently in`),
		},
		{
			value: 'open_dms_and_all_guilds',
			label: i18n._(msg`Open DMs + Communities`),
			description: i18n._(msg`Across all DMs you currently have open + all Communities you're currently in`),
		},
	];

	const GUILD_SCOPE_OPTIONS: Array<ScopeValueOption> = [
		{
			value: 'current',
			label: i18n._(msg`Current Community`),
			isDefault: true,
			description: i18n._(msg`Search only in the current Community`),
		},
		{
			value: 'all_guilds',
			label: i18n._(msg`All Communities`),
			description: i18n._(msg`Across all Communities you're currently in`),
		},
		{
			value: 'all_dms',
			label: i18n._(msg`All DMs Only`),
			description: i18n._(msg`Across all DMs you've ever been in only`),
		},
		{
			value: 'open_dms',
			label: i18n._(msg`Open DMs Only`),
			description: i18n._(msg`Across all DMs you currently have open only`),
		},
		{
			value: 'all',
			label: i18n._(msg`All DMs + Communities`),
			description: i18n._(msg`Across all DMs you've ever been in + all Communities you're currently in`),
		},
		{
			value: 'open_dms_and_all_guilds',
			label: i18n._(msg`Open DMs + Communities`),
			description: i18n._(msg`Across all DMs you currently have open + all Communities you're currently in`),
		},
	];

	if (!channel) {
		return GUILD_SCOPE_OPTIONS;
	}

	const isDmChannel = channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM;
	return isDmChannel ? DM_SCOPE_OPTIONS : GUILD_SCOPE_OPTIONS;
};
