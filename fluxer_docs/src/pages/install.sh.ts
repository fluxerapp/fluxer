// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIRoute} from 'astro';
import {installerScriptResponse, shellInstaller} from '../installer/Installer';

export const prerender = false;

export const GET: APIRoute = () => installerScriptResponse(shellInstaller);
