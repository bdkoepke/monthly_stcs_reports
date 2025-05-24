module Search

open Lucene.Net.Analysis.Standard
open Lucene.Net.Documents
open Lucene.Net.Index
open Lucene.Net.Search
open Lucene.Net.Store
open Lucene.Net.Util

type Hit =
    { ScoreDoc: ScoreDoc
      Document: Document }

type Match = { Name: string; Hits: array<Hit> }

let createAndIndexAnalyzer (version: Version) (directory: Directory) (documents: array<Document>) =
    let analyzer = new StandardAnalyzer(version)

    using (new IndexWriter(directory, analyzer, true, IndexWriter.MaxFieldLength.UNLIMITED)) (fun writer ->
        documents |> Array.iter writer.AddDocument)

    analyzer
