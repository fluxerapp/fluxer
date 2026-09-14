import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {type ConfigObject, isConfigObject} from '@fluxer/config/src/config_loader/ConfigObject';
import {parse} from 'yaml';

const SELF_HOSTING = fileURLToPath(new URL('../../../../deploy/self-hosting/', import.meta.url));

function readSelfHostingFile(name: string): string {
	return readFileSync(path.join(SELF_HOSTING, name), 'utf8');
}

function mapping(value: unknown, name: string): ConfigObject {
	assert.ok(isConfigObject(value), `${name} must be a mapping`);
	return value;
}

const compose = mapping(parse(readSelfHostingFile('docker-compose.yml'), {merge: true}), 'docker-compose.yml');
const services = mapping(compose.services, 'services');
export const serviceNames = Object.keys(services);
assert.ok(serviceNames.length > 0, 'docker-compose.yml must define services');

export function composeService(name: string): ConfigObject {
	return mapping(services[name], `services.${name}`);
}

function environment(value: unknown, name: string): Record<string, string> {
	return Object.fromEntries(
		Object.entries(mapping(value, name)).map(([key, entry]) => {
			assert.ok(
				typeof entry === 'string' || typeof entry === 'number' || typeof entry === 'boolean',
				`${name}.${key} must have an explicit scalar value`,
			);
			return [key, String(entry)];
		}),
	);
}

export function serviceEnvironment(name: string): Record<string, string> {
	const value = composeService(name).environment;
	return value === undefined ? {} : environment(value, `services.${name}.environment`);
}

export const sharedEnvironment = environment(compose['x-fluxer-env'], 'x-fluxer-env');

export function serviceList(name: string, key: string): Array<string> {
	const value = composeService(name)[key];
	assert.ok(Array.isArray(value), `services.${name}.${key} must be a list`);
	assert.ok(
		value.every((item) => typeof item === 'string'),
		`services.${name}.${key} must contain strings`,
	);
	return value;
}
