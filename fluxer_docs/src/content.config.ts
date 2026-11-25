// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineCollection} from 'astro:content';
import {docsLoader} from '@astrojs/starlight/loaders';
import {docsSchema} from '@astrojs/starlight/schema';
import {z} from 'astro/zod';

export const collections = {
	docs: defineCollection({
		loader: docsLoader(),
		schema: docsSchema({extend: z.object({tag: z.string().optional()})}),
	}),
};
