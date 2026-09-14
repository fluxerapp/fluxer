// SPDX-License-Identifier: AGPL-3.0-or-later
const PATH_PARAMETER_PATTERN = /:(\w+)/g;

export function extractPathParameterNames(path: string): Array<string> {
	return Array.from(path.matchAll(PATH_PARAMETER_PATTERN), (match) => match[1]);
}

export function convertPathToOpenAPI(path: string): string {
	return path.replace(PATH_PARAMETER_PATTERN, '{$1}');
}
