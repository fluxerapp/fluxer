// SPDX-License-Identifier: AGPL-3.0-or-later

const MEDIA_PROXY_IMAGE_SIZE_QUERY_VALUES = [
	'16',
	'20',
	'22',
	'24',
	'28',
	'32',
	'40',
	'44',
	'48',
	'56',
	'60',
	'64',
	'80',
	'96',
	'100',
	'128',
	'160',
	'240',
	'256',
	'300',
	'320',
	'480',
	'512',
	'600',
	'640',
	'1024',
	'1280',
	'1536',
	'2048',
	'3072',
	'4096',
	'8192',
	'16384',
] as const;

export type MediaProxyImageSizeQueryValue = (typeof MEDIA_PROXY_IMAGE_SIZE_QUERY_VALUES)[number];
type ParseNumericLiteral<T extends string> = T extends `${infer N extends number}` ? N : never;
export type MediaProxyImageSize = ParseNumericLiteral<MediaProxyImageSizeQueryValue>;

export const MEDIA_PROXY_IMAGE_SIZES: ReadonlyArray<MediaProxyImageSize> = MEDIA_PROXY_IMAGE_SIZE_QUERY_VALUES.map(
	(value) => Number(value) as MediaProxyImageSize,
);
