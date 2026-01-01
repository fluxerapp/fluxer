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

export const calculateAge = (dateOfBirth: {year: number; month: number; day: number} | string): number => {
	const today = new Date();

	let birthDate: Date;

	if (typeof dateOfBirth === 'string') {
		const [year, month, day] = dateOfBirth.split('-').map(Number);
		birthDate = new Date(year, month - 1, day);
	} else {
		birthDate = new Date(dateOfBirth.year, dateOfBirth.month - 1, dateOfBirth.day);
	}

	const age = today.getFullYear() - birthDate.getFullYear();
	const monthDiff = today.getMonth() - birthDate.getMonth();
	const dayDiff = today.getDate() - birthDate.getDate();

	return monthDiff < 0 || (monthDiff === 0 && dayDiff < 0) ? age - 1 : age;
};

export const isUserAdult = (dateOfBirth?: {year: number; month: number; day: number} | string | null): boolean => {
	if (!dateOfBirth) return false;
	return calculateAge(dateOfBirth) >= 18;
};
