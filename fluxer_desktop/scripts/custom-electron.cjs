const {execFile} = require('node:child_process');
const {createHash} = require('node:crypto');
const {createReadStream, statSync} = require('node:fs');
const fs = require('node:fs/promises');
const path = require('node:path');
const {promisify} = require('node:util');

const execFileAsync = promisify(execFile);
const expectedElectronVersion = '44.9.1';
const expectedElectronRevision = 'e02265be96c5e13ae43644ef482b718e1e800499';
const chromiumPatchPath = path.join(
	__dirname,
	'../electron/patches/fix_synchronize_vaapi_surfaces_before_output.patch',
);

function resolveLinuxElectronDist() {
	const configuredDirectory = process.env.FLUXER_ELECTRON_DIST?.trim();
	if (!configuredDirectory) {
		throw new Error(
			'Linux packaging requires patched Electron. Set FLUXER_ELECTRON_DIST to the directory containing the custom Electron ZIP and its .fluxer.json build manifest.',
		);
	}
	const directory = path.resolve(configuredDirectory);
	if (!statSync(directory, {throwIfNoEntry: false})?.isDirectory()) {
		throw new Error(`FLUXER_ELECTRON_DIST is not a directory: ${directory}`);
	}
	return directory;
}

async function sha256File(filePath) {
	const hash = createHash('sha256');
	for await (const chunk of createReadStream(filePath)) hash.update(chunk);
	return hash.digest('hex');
}

const inspectArchive = `
import hashlib, json, stat, struct, sys, zipfile
archive_path, arch, version = sys.argv[1:]
with zipfile.ZipFile(archive_path) as archive:
    for required in ('electron', 'version', 'LICENSE', 'LICENSES.chromium.html'):
        matches = [entry for entry in archive.infolist() if entry.filename == required]
        if len(matches) != 1 or matches[0].is_dir() or stat.S_ISLNK(matches[0].external_attr >> 16):
            raise ValueError('Expected one regular ZIP entry: ' + required)
    if archive.read('version').decode('utf-8').strip() != version:
        raise ValueError('Electron ZIP version does not match ' + version)
    with archive.open('electron') as executable:
        header = executable.read(20)
        if len(header) != 20 or header[:6] != b'\\x7fELF\\x02\\x01':
            raise ValueError('Electron must be a 64-bit little-endian ELF executable')
        if struct.unpack_from('<H', header, 18)[0] != {'x64': 62, 'arm64': 183}[arch]:
            raise ValueError('Electron ELF architecture does not match ' + arch)
        digest = hashlib.sha256(header)
        marker = b'failed to synchronize VA surface before output'
        tail = header
        has_patch_marker = marker in header
        while chunk := executable.read(1024 * 1024):
            digest.update(chunk)
            window = tail + chunk
            has_patch_marker = has_patch_marker or marker in window
            tail = window[-(len(marker) - 1):]
        if not has_patch_marker:
            raise ValueError('Electron executable is missing the VA-API synchronization patch marker')
print(json.dumps({'binarySha256': digest.hexdigest()}))
`;

async function verifyLinuxElectronArchive(directory, arch) {
	if (!['x64', 'arm64'].includes(arch)) {
		throw new Error(`Custom Linux Electron does not support architecture: ${arch}`);
	}
	const artifactName = `electron-v${expectedElectronVersion}-linux-${arch}`;
	const archiveName = `${artifactName}.zip`;
	const manifestPath = path.join(directory, `${artifactName}.fluxer.json`);
	let manifest;
	try {
		manifest = JSON.parse(await fs.readFile(manifestPath, 'utf8'));
	} catch (error) {
		throw new Error(`Cannot read custom Electron build manifest ${manifestPath}: ${error.message}`);
	}
	const expectedFields = {
		schemaVersion: 1,
		electronVersion: expectedElectronVersion,
		electronRevision: expectedElectronRevision,
		platform: 'linux',
		arch,
		archiveName,
	};
	for (const [field, expected] of Object.entries(expectedFields)) {
		if (manifest?.[field] !== expected) {
			throw new Error(`Custom Electron manifest ${field} must be ${JSON.stringify(expected)}: ${manifestPath}`);
		}
	}
	if ((await sha256File(chromiumPatchPath)) !== manifest?.chromiumPatchSha256) {
		throw new Error(`Custom Electron manifest chromiumPatchSha256 does not match the current patch: ${manifestPath}`);
	}
	if (!/^[a-f0-9]{64}$/.test(manifest.archiveSha256)) {
		throw new Error(`Custom Electron manifest has no valid archiveSha256: ${manifestPath}`);
	}
	const archivePath = path.join(directory, archiveName);
	if ((await sha256File(archivePath)) !== manifest.archiveSha256) {
		throw new Error(`Custom Electron ZIP checksum does not match its build manifest: ${archivePath}`);
	}
	let inspection;
	try {
		const {stdout} = await execFileAsync(
			'python3',
			['-c', inspectArchive, archivePath, arch, expectedElectronVersion],
			{
				maxBuffer: 64 * 1024,
			},
		);
		inspection = JSON.parse(stdout);
	} catch (error) {
		throw new Error(`Cannot verify custom Electron ZIP ${archivePath}: ${error.stderr?.trim() || error.message}`);
	}
	if (!/^[a-f0-9]{64}$/.test(inspection.binarySha256)) {
		throw new Error(`Custom Electron ZIP inspection returned no executable checksum: ${archivePath}`);
	}
	return {manifest, binarySha256: inspection.binarySha256};
}

async function verifyPackagedLinuxElectron(executablePath, artifact) {
	if (!artifact) throw new Error('Custom Electron archive was not verified before Linux packaging.');
	if ((await sha256File(executablePath)) !== artifact.binarySha256) {
		throw new Error(`Packaged Linux executable does not match the verified custom Electron build: ${executablePath}`);
	}
}

module.exports = {
	expectedElectronVersion,
	resolveLinuxElectronDist,
	verifyLinuxElectronArchive,
	verifyPackagedLinuxElectron,
};
