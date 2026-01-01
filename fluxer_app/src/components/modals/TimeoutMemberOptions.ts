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

export interface TimeoutDurationOption {
	value: number;
	label: string;
}

interface TimeoutDurationOptionDescriptor {
	value: number;
	label: MessageDescriptor;
}

const TIMEOUT_DURATION_OPTIONS_DESCRIPTORS: ReadonlyArray<TimeoutDurationOptionDescriptor> = [
	{value: 60, label: msg`60 seconds`},
	{value: 5 * 60, label: msg`5 minutes`},
	{value: 10 * 60, label: msg`10 minutes`},
	{value: 60 * 60, label: msg`1 hour`},
	{value: 60 * 60 * 24, label: msg`1 day`},
	{value: 60 * 60 * 24 * 7, label: msg`1 week`},
];

export const getTimeoutDurationOptions = (
	t: (msg: MessageDescriptor) => string,
): ReadonlyArray<TimeoutDurationOption> => {
	return TIMEOUT_DURATION_OPTIONS_DESCRIPTORS.map((opt) => ({
		...opt,
		label: t(opt.label),
	}));
};
