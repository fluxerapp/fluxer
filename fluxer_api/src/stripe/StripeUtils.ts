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

export function extractId(value: string | {id: string} | null | undefined): string | null {
	if (!value) return null;
	if (typeof value === 'string') return value || null;
	return value.id || null;
}

export function addMonthsClamp(date: Date, months: number): Date {
	const d = new Date(date);
	const day = d.getDate();
	d.setMonth(d.getMonth() + months);
	if (d.getDate() < day) {
		d.setDate(0);
	}
	return d;
}
