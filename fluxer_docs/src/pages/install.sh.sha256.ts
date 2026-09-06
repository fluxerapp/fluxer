// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIRoute} from 'astro';
import {installerChecksumResponse, shellInstaller} from '../installer/Installer';

export const prerender = false;

export const GET: APIRoute = () => installerChecksumResponse(shellInstaller);
