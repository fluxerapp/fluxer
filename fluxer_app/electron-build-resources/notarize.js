/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

const {notarize} = require('@electron/notarize');

exports.default = async function notarizing(context) {
	const {electronPlatformName, appOutDir} = context;

	if (electronPlatformName !== 'darwin') {
		return;
	}

	if (!process.env.APPLE_ID || !process.env.APPLE_APP_SPECIFIC_PASSWORD || !process.env.APPLE_TEAM_ID) {
		console.log('Skipping notarization: Apple credentials not set');
		return;
	}

	const appName = context.packager.appInfo.productFilename;
	const appPath = `${appOutDir}/${appName}.app`;

	console.log(`Notarizing ${appPath}...`);

	try {
		await notarize({
			tool: 'notarytool',
			appPath,
			appleId: process.env.APPLE_ID,
			appleIdPassword: process.env.APPLE_APP_SPECIFIC_PASSWORD,
			teamId: process.env.APPLE_TEAM_ID,
		});
		console.log('Notarization complete');
	} catch (error) {
		console.error('Notarization failed:', error);
		throw error;
	}
};
