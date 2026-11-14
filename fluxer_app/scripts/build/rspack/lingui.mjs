// SPDX-License-Identifier: AGPL-3.0-or-later

export function getLinguiSwcPluginConfig() {
	return [
		'@lingui/swc-plugin',
		{
			descriptorFields: 'all',
			runtimeModules: {
				i18n: ['@lingui/core', 'i18n'],
				trans: ['@lingui/react', 'Trans'],
			},
		},
	];
}
