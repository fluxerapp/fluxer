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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export interface MuteDurationOption {
	label: string;
	value: number | null;
}

interface MuteDurationOptionDescriptor {
	label: MessageDescriptor;
	value: number | null;
}

const MUTE_DURATION_OPTIONS_DESCRIPTORS: Array<MuteDurationOptionDescriptor> = [
	{label: msg`For 15 Minutes`, value: 15 * 60 * 1000},
	{label: msg`For 1 Hour`, value: 60 * 60 * 1000},
	{label: msg`For 3 Hours`, value: 3 * 60 * 60 * 1000},
	{label: msg`For 8 Hours`, value: 8 * 60 * 60 * 1000},
	{label: msg`For 24 Hours`, value: 24 * 60 * 60 * 1000},
	{label: msg`Until I turn it back on`, value: null},
];

export const getMuteDurationOptions = (t: (msg: MessageDescriptor) => string): Array<MuteDurationOption> => {
	return MUTE_DURATION_OPTIONS_DESCRIPTORS.map((opt) => ({
		...opt,
		label: t(opt.label),
	}));
};

export const createMuteConfig = (value: number | null) =>
	value == null
		? null
		: {
				selected_time_window: value,
				end_time: new Date(Date.now() + value).toISOString(),
			};
