// SPDX-License-Identifier: AGPL-3.0-or-later

import {collectBodyFieldPaths} from '@fluxer/schema/src/primitives/Base64BinaryFields';
import type {ZodType} from 'zod';

const bodySchemas = new Set<ZodType>();
const binaryPaths = new Set<string>();

export const requestBodySchemas: ReadonlySet<ZodType> = bodySchemas;
export const binaryRequestBodyPaths: ReadonlySet<string> = binaryPaths;

export function registerRequestBodySchema(schema: ZodType): void {
	if (bodySchemas.has(schema)) return;
	bodySchemas.add(schema);
	for (const path of collectBodyFieldPaths(schema).binary) {
		binaryPaths.add(path);
	}
}
