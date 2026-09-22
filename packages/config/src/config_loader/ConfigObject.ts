export type ConfigObject = Record<string, unknown>;

export function isConfigObject(value: unknown): value is ConfigObject {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}
