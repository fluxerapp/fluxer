// SPDX-License-Identifier: AGPL-3.0-or-later

const LOCAL_DATE_PATTERN = /^(-?\d+)-(\d{1,2})-(\d{1,2})$/;

export class LocalDate {
	readonly year: number;
	readonly month: number;
	readonly day: number;

	constructor(year: number, month: number, day: number) {
		this.year = year;
		this.month = month;
		this.day = day;
	}

	static fromString(value: string): LocalDate {
		const match = LOCAL_DATE_PATTERN.exec(value);
		if (!match) {
			throw new Error(`Invalid local date: ${value}`);
		}
		return new LocalDate(Number(match[1]), Number(match[2]), Number(match[3]));
	}

	toString(): string {
		const sign = this.year < 0 ? '-' : '';
		const year = String(Math.abs(this.year)).padStart(4, '0');
		return `${sign}${year}-${String(this.month).padStart(2, '0')}-${String(this.day).padStart(2, '0')}`;
	}
}
