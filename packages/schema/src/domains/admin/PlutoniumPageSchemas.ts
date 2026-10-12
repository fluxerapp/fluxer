// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

export const PlutoniumPageAssignmentResponse = z.object({
	enabled: z.boolean(),
});

export type PlutoniumPageAssignmentResponse = z.infer<typeof PlutoniumPageAssignmentResponse>;
