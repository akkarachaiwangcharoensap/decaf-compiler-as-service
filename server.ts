import express from 'express'
import cors from 'cors'

import { json } from 'body-parser'
import { writeFile, mkdir, readFile } from 'fs/promises'
import { existsSync } from 'fs'
import { exec } from 'child_process'
import { tmpdir } from 'os'
import { join, resolve } from 'path'
import { promisify } from 'util'

const execAsync = promisify(exec)
const compilerDir = resolve(process.cwd(), 'src/compiler')
const app = express()

app.use(cors({
  origin: '*',
  methods: ['POST', 'GET'],
  allowedHeaders: ['Content-Type']
}))

app.use(json())

app.post('/compile', async (req, res) => {
    try {
        const { code } = req.body
        const tempDir = join(tmpdir(), 'decaf')
        const filename = 'temp.decaf'
        const filepath = join(tempDir, filename)
        const outPrefix = join(tempDir, 'temp')

        if (!existsSync(tempDir)) await mkdir(tempDir, { recursive: true })
        await writeFile(filepath, code)

        const llvmRunPath = join(compilerDir, 'llvm-run')
        const llvmRunCmd = `${llvmRunPath} ${filepath} ${tempDir}`

        const { stdout, stderr } = await execAsync(llvmRunCmd, {
            cwd: compilerDir
        })

        const outputPath = `${outPrefix}.run.out`
        const errPath = `${outPrefix}.run.err`
        const output = existsSync(outputPath) ? await readFile(outputPath, 'utf-8') : ''
        const error = existsSync(errPath) ? await readFile(errPath, 'utf-8') : ''

        res.json({ output, error })
    } catch (e: any) {
        res.status(500).json({ error: e.message || 'Unexpected server error' })
    }
})

const PORT = process.env.PORT || 3000
app.listen(PORT, () => console.log(`Server running on port ${PORT}`))