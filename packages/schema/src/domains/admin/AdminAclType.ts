// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {z} from 'zod';

export const AdminAclType = z.enum(AdminACLs);

export const ADMIN_ACL_COUNT = AdminAclType.options.length;

export type AdminAclType = z.infer<typeof AdminAclType>;
