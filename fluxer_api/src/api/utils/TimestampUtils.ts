const MAX_DATE_TIMESTAMP = 8640000000000000;

export function isValidTimestamp(timestamp: unknown): timestamp is number {
	return typeof timestamp === 'number' && Number.isInteger(timestamp) && Math.abs(timestamp) <= MAX_DATE_TIMESTAMP;
}

function validateTimestamp(timestamp: number, fieldName: string): number {
	if (!isValidTimestamp(timestamp)) {
		throw new RangeError(`${fieldName} must be an integer within the valid Date range`);
	}
	return timestamp;
}

export function getValidTimestamp(date: Date, fieldName: string): number {
	return validateTimestamp(date.getTime(), fieldName);
}

export function parseStoredTimestamp(value: string | null, fieldName: string): number | null {
	if (value === null) return null;
	const digits = value.startsWith('-') ? value.slice(1) : value;
	if (digits.length === 0 || /\D/.test(digits)) {
		throw new TypeError(`${fieldName} must be a decimal millisecond timestamp`);
	}
	return validateTimestamp(Number(value), fieldName);
}
