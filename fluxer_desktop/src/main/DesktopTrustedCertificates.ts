// SPDX-License-Identifier: AGPL-3.0-or-later

import {X509Certificate} from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import tls from 'node:tls';
import {createChildLogger} from '@electron/common/Logger';

const logger = createChildLogger('DesktopTrustedCertificates');

export type DesktopCertificateStore = 'default' | 'system';
export type DesktopCertificateSource = (store: DesktopCertificateStore) => ReadonlyArray<string>;

function readStore(source: DesktopCertificateSource, store: DesktopCertificateStore): ReadonlyArray<string> {
	try {
		return source(store);
	} catch (error) {
		logger.warn('Could not read a certificate store, continuing without it', {store, error});
		return [];
	}
}

export function resolveDesktopTrustedCertificates(
	source: DesktopCertificateSource = (store) => tls.getCACertificates(store),
): ReadonlyArray<string> {
	const merged = new Set<string>();
	for (const certificate of readStore(source, 'default')) merged.add(certificate);
	for (const certificate of readStore(source, 'system')) merged.add(certificate);
	return [...merged];
}

interface DistroTrustLayout {
	readonly bundle: string;
	readonly anchorDirectories: ReadonlyArray<string>;
}

const DISTRO_TRUST_LAYOUTS: ReadonlyArray<DistroTrustLayout> = Object.freeze([
	{bundle: 'etc/ssl/certs/ca-certificates.crt', anchorDirectories: ['usr/local/share/ca-certificates']},
	{
		bundle: 'etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem',
		anchorDirectories: ['etc/pki/ca-trust/source/anchors', 'usr/share/pki/ca-trust-source/anchors'],
	},
	{
		bundle: 'etc/ca-certificates/extracted/tls-ca-bundle.pem',
		anchorDirectories: ['etc/ca-certificates/trust-source/anchors', 'usr/share/ca-certificates/trust-source/anchors'],
	},
	{
		bundle: 'var/lib/ca-certificates/ca-bundle.pem',
		anchorDirectories: ['etc/pki/trust/anchors', 'usr/share/pki/trust/anchors'],
	},
]);
const PEM_BLOCK = /-----BEGIN CERTIFICATE-----[\s\S]+?-----END CERTIFICATE-----/gu;
const MAX_ANCHOR_DEPTH = 4;
const MAX_ANCHOR_FILES = 512;
const MAX_ANCHOR_FILE_BYTES = 1024 * 1024;

function pemBody(pem: string): string {
	return pem.replace(/-----[^-]+-----|\s/gu, '');
}

function readBundle(file: string): ReadonlySet<string> | null {
	try {
		return new Set((fs.readFileSync(file, 'utf8').match(PEM_BLOCK) ?? []).map(pemBody));
	} catch {
		return null;
	}
}

function anchorFiles(directory: string, depth: number, files: Array<string>): void {
	let entries: Array<fs.Dirent>;
	try {
		entries = fs.readdirSync(directory, {withFileTypes: true});
	} catch {
		return;
	}
	for (const entry of entries) {
		if (files.length >= MAX_ANCHOR_FILES) return;
		const target = path.join(directory, entry.name);
		if (entry.isDirectory()) {
			if (depth < MAX_ANCHOR_DEPTH) anchorFiles(target, depth + 1, files);
		} else if (entry.isFile() || entry.isSymbolicLink()) {
			files.push(target);
		}
	}
}

function readAnchorFile(file: string): ReadonlyArray<X509Certificate> {
	try {
		const stat = fs.statSync(file);
		if (!stat.isFile() || stat.size > MAX_ANCHOR_FILE_BYTES) return [];
		const content = fs.readFileSync(file);
		const blocks = content.toString('latin1').match(PEM_BLOCK);
		if (blocks == null) return [new X509Certificate(content)];
		return blocks.flatMap((block) => {
			try {
				return [new X509Certificate(block)];
			} catch {
				return [];
			}
		});
	} catch {
		return [];
	}
}

export function resolveDesktopLocallyAddedCertificates(root: string = '/'): ReadonlyArray<string> {
	const added = new Map<string, string>();
	for (const layout of DISTRO_TRUST_LAYOUTS) {
		const bundle = readBundle(path.join(root, layout.bundle));
		if (bundle == null) continue;
		const files: Array<string> = [];
		for (const directory of layout.anchorDirectories) anchorFiles(path.join(root, directory), 0, files);
		for (const certificate of files.flatMap(readAnchorFile)) {
			const body = certificate.raw.toString('base64');
			if (bundle.has(body)) added.set(body, certificate.toString());
		}
	}
	return [...added.values()];
}
