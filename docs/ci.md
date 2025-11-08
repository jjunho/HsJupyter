# Continuous Integration Documentation

This document describes the CI/CD pipelines configured for the HsJupyter project.

## Workflows

### 1. CI (`.github/workflows/ci.yml`)

**Triggers**: Push to `master`/`main`, Pull Requests

**Jobs**:

#### Build and Test

- **Platforms**: Ubuntu Latest
- **GHC Versions**: 9.6.7
- **Cabal Versions**: 3.10

**Steps**:

1. Checkout code
2. Setup Haskell toolchain
3. Install system dependencies (libzmq3-dev)
4. Cache cabal packages and build artifacts
5. Build project with `-O0` for speed
6. Run all test suites
7. Run `cabal check` for package validation
8. Generate Haddock documentation

#### Lint Check

- Runs HLint on source code
- Configuration in `.hlint.yaml`
- Non-blocking (warnings don't fail build)

#### Markdown Lint

- Validates all Markdown files
- Configuration in `.markdownlint.json`
- Ensures documentation quality

### 2. Release (`.github/workflows/release.yml`)

**Triggers**: Git tags matching `v*.*.*` (e.g., `v0.1.0`)

**Steps**:

1. Build optimized binary (`-O2`)
2. Create Linux x86_64 tarball
3. Generate SHA256 checksums
4. Create GitHub Release with:
   - Binary artifacts
   - Installation instructions
   - Changelog reference

**Usage**:

```bash
# Create a release
git tag v0.1.0
git push origin v0.1.0
```

### 3. PR Checks (`.github/workflows/pr-checks.yml`)

**Triggers**: Pull Request events

**Jobs**:

#### Code Quality

- Validates commit message format (Conventional Commits)
- Checks for TODO/FIXME comments
- Validates file sizes (<500KB)

#### Security Scan

- Runs Trivy security scanner
- Uploads results to GitHub Security tab
- Scans for vulnerabilities in dependencies

#### Test Coverage

- Runs tests with coverage enabled
- Generates coverage reports
- Posts summary to PR

## Local CI Testing

### Development Build and Test

```bash
# Fast development build
cabal build all -O0

# Run all tests
cabal test all --test-show-details=streaming

# Check package
cabal check
```

### Linting

```bash
# Install HLint
cabal install hlint

# Run HLint
hlint src/ app/ test/
```

### Markdown Linting

```bash
# Install markdownlint-cli
npm install -g markdownlint-cli

# Run markdownlint
markdownlint '**/*.md' --ignore node_modules --ignore dist-newstyle
```

## Caching Strategy

The CI uses GitHub Actions cache for:

- `~/.cabal/store` - Cabal package store
- `~/.cabal/packages` - Package index
- `dist-newstyle` - Build artifacts

**Cache Key**: `${{ runner.os }}-cabal-${{ matrix.ghc }}-${{ hashFiles('**/*.cabal', 'cabal.project') }}`

This significantly speeds up builds (5-10 minutes → 2-3 minutes).

## Conventional Commits

All commits must follow the Conventional Commits specification:

```text
type(scope): description

[optional body]

[optional footer]
```

**Types**:

- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `perf`: Performance improvements
- `test`: Test additions or changes
- `chore`: Build process or auxiliary tool changes
- `ci`: CI configuration changes
- `build`: Build system changes
- `revert`: Revert previous commit

**Examples**:

```text
feat(runtime): add persistent GHC sessions
fix(cli): resolve import path resolution
docs(readme): update installation instructions
chore(ci): add GitHub Actions workflows
```

## Badge Status

Add these badges to your README:

```markdown
[![CI](https://github.com/jjunho/HsJupyter/workflows/CI/badge.svg)](https://github.com/jjunho/HsJupyter/actions/workflows/ci.yml)
[![Release](https://github.com/jjunho/HsJupyter/workflows/Release/badge.svg)](https://github.com/jjunho/HsJupyter/actions/workflows/release.yml)
```

## Troubleshooting

### Build Failures

1. **Dependency issues**: Clear cache and rebuild

   ```bash
   rm -rf ~/.cabal/store dist-newstyle
   cabal update
   cabal build all
   ```

2. **Test failures**: Run locally to reproduce

   ```bash
   cabal test all --test-show-details=streaming
   ```

3. **HLint errors**: Fix suggestions or add exceptions to `.hlint.yaml`

### Release Issues

1. **Tag not triggering release**: Ensure tag follows `v*.*.*` format
2. **Binary not found**: Check `cabal list-bin` output path

## Future Improvements

- [ ] Multi-platform builds (macOS, Windows)
- [ ] Binary caching across runs
- [ ] Automated dependency updates (Dependabot)
- [ ] Performance benchmarking in CI
- [ ] Docker image builds
- [ ] Nightly builds from master

