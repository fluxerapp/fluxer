// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {z} from 'zod';

type AdminACL = (typeof AdminACLs)[keyof typeof AdminACLs];

const ADMIN_ACL_VALUES = Object.values(AdminACLs) as [AdminACL, ...Array<AdminACL>];

export const ADMIN_ACL_COUNT = ADMIN_ACL_VALUES.length;

export const AdminAclType = z.enum(ADMIN_ACL_VALUES);

export type AdminAclType = z.infer<typeof AdminAclType>;
