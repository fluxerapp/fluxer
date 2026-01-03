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

/** @type {import('electron-builder').Configuration} */
const config = (() => {
	const channel = process.env.BUILD_CHANNEL === 'canary' ? 'canary' : 'stable';
	const isCanary = channel === 'canary';

	const appId = isCanary ? 'app.fluxer.canary' : 'app.fluxer';
	const productName = isCanary ? 'Fluxer Canary' : 'Fluxer';
	const iconsDir = isCanary ? 'electron-build-resources/icons-canary' : 'electron-build-resources/icons-stable';

	const macEntitlements = isCanary
		? 'electron-build-resources/entitlements.mac.canary.plist'
		: 'electron-build-resources/entitlements.mac.stable.plist';

	const macProfile = isCanary
		? 'electron-build-resources/profiles/Fluxer_Canary.provisionprofile'
		: 'electron-build-resources/profiles/Fluxer.provisionprofile';

	const winIconUrl = isCanary
		? 'https://fluxerstatic.com/web/icons/desktop/canary/icon.ico'
		: 'https://fluxerstatic.com/web/icons/desktop/stable/icon.ico';

	const linuxExecutableName = isCanary ? 'fluxercanary' : 'fluxer';
	const linuxSynopsis = productName;
	const linuxDescription = productName;

	return {
		appId,
		productName,
		copyright: 'Copyright (C) 2026 Fluxer Contributors',

		artifactName: `fluxer-${channel}-\${version}-\${arch}.\${ext}`,

		directories: {
			output: 'dist-electron',
			buildResources: 'electron-build-resources',
		},

		files: [
			'src-electron/dist/**/*',
			'!**/*.map',
			'!**/*.md',
			'!**/README*',
			'!**/readme*',
			'!**/CHANGELOG*',
			'!**/LICENSE*',
			'!**/.github/**',
			'!**/docs/**',
			'!**/doc/**',
			'!**/example/**',
			'!**/examples/**',
			'!**/test/**',
			'!**/tests/**',
			'!**/__tests__/**',
			'!**/*.ts',
			'!**/tsconfig*.json',
		],

		extraMetadata: {
			main: 'src-electron/dist/main/index.js',
			type: 'module',
		},

		asar: true,
		compression: 'normal',

		asarUnpack: [
			'**/*.node',
			'**/node_modules/uiohook-napi/**',
			'**/node_modules/input-monitoring-check/**',
			'**/src-electron/dist/preload/**',
		],

		extraResources: [
			{from: `${iconsDir}/512x512.png`, to: '512x512.png'},
			{from: `${iconsDir}/badges`, to: 'badges'},
			{from: `${iconsDir}/_compiled/Assets.car`, to: 'Assets.car'},
		],

		mac: {
			category: 'public.app-category.social-networking',
			icon: `${iconsDir}/_compiled/AppIcon.icns`,
			hardenedRuntime: true,
			gatekeeperAssess: false,
			entitlements: macEntitlements,
			entitlementsInherit: 'electron-build-resources/entitlements.mac.inherit.plist',
			provisioningProfile: macProfile,
			extendInfo: {
				CFBundleIconName: 'AppIcon',
				NSMicrophoneUsageDescription: 'Fluxer needs access to your microphone for voice chat.',
				NSCameraUsageDescription: 'Fluxer needs access to your camera for video chat.',
				NSInputMonitoringUsageDescription: 'Fluxer needs Input Monitoring access for global shortcuts and hotkeys.',
			},
			notarize: true,
			target: [
				{target: 'dmg', arch: ['x64', 'arm64']},
				{target: 'zip', arch: ['x64', 'arm64']},
			],
		},

		dmg: {
			sign: false,
			icon: `${iconsDir}/_compiled/AppIcon.icns`,
			format: 'UDZO',
			contents: [
				{x: 130, y: 220},
				{x: 410, y: 220, type: 'link', path: '/Applications'},
			],
		},

		win: {
			icon: `${iconsDir}/icon.ico`,
			target: [{target: 'squirrel'}],
		},

		squirrelWindows: {
			iconUrl: winIconUrl,
		},

		linux: {
			icon: iconsDir,
			category: 'Network',
			maintainer: 'Fluxer Contributors',
			synopsis: linuxSynopsis,
			description: linuxDescription,
			executableName: linuxExecutableName,
			target: ['dir', 'AppImage', 'deb', 'rpm', 'tar.gz'],
			mimeTypes: ['x-scheme-handler/fluxer'],
		},

		protocols: [{name: 'Fluxer', schemes: ['fluxer']}],
	};
})();

module.exports = config;
