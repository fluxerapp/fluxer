import {SchemaRegistry} from '@fluxer/openapi/src/registry/SchemaRegistry';
import {
	CreateVoiceServerRequestBody,
	UpdateVoiceRegionRequest,
	UpdateVoiceRegionRequestBody,
} from '@fluxer/schema/src/domains/admin/AdminVoiceSchemas';
import {SudoVerificationSchema} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {describe, expect, it} from 'vitest';
import {z} from 'zod';

describe('request schema registry', () => {
	it('checks the empty object supplied by the JSON body reader', () => {
		const registry = new SchemaRegistry();
		registry.registerZod('SudoVerification', SudoVerificationSchema);
		registry.registerZod('UpdateVoiceRegion', UpdateVoiceRegionRequest);
		registry.registerZod('UpdateVoiceRegionBody', UpdateVoiceRegionRequestBody);
		registry.registerZod('CreateVoiceServerBody', CreateVoiceServerRequestBody);
		expect(registry.acceptsEmptyObject('SudoVerification')).toBe(true);
		expect(registry.acceptsEmptyObject('UpdateVoiceRegion')).toBe(false);
		expect(registry.acceptsEmptyObject('UpdateVoiceRegionBody')).toBe(true);
		expect(registry.acceptsEmptyObject('CreateVoiceServerBody')).toBe(false);
	});

	it('does not treat optional properties as proof that an empty body is valid', () => {
		const registry = new SchemaRegistry();
		registry.registerZod(
			'Edit',
			z.object({name: z.string().optional()}).refine((value) => value.name !== undefined),
		);
		expect(registry.acceptsEmptyObject('Edit')).toBe(false);
	});

	it('rejects missing schemas and overlapping static definitions', () => {
		const registry = new SchemaRegistry();
		expect(() => registry.getRef('Missing')).toThrow('Unknown OpenAPI schema: Missing');
		registry.register('Text', {type: 'string'});
		expect(() => registry.registerZod('Text', z.string())).toThrow('Zod schema also has an OpenAPI definition: Text');
	});
});
