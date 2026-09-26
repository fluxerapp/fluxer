export function readOptionalIntegerEnv(name: string): number | undefined {
	const raw = process.env[name]?.trim();
	if (raw === undefined || raw === '') return undefined;
	const value = Number(raw);
	if (!/^-?\d+$/.test(raw) || !Number.isSafeInteger(value)) {
		throw new Error(`${name} must be a safe integer`);
	}
	return value;
}

export function requireIntegerInRange(name: string, value: number, min: number, max: number): number {
	if (!Number.isSafeInteger(value) || value < min || value > max) {
		throw new RangeError(`${name} must be an integer between ${min} and ${max}`);
	}
	return value;
}
